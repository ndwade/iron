package io.github.iltotore.iron

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.Positive

//Opaque types are truly opaque when used in another file than the one where they're defined. See Scala documentation.
opaque type Temperature = Double :| Positive
object Temperature extends RefinedTypeOps[Temperature]

type Moisture = Double :| Positive
object Moisture extends RefinedTypeOps[Moisture]

/**
 * confounding examples
 */
import constraint.string.*
import constraint.numeric.*

import _root_.cats.syntax.eq.*
import cats.given

object numer:

  val s: String :| (Trimmed & Alphanumeric) = "foo"
  val t: String :| Trimmed = "bar"

  val b = s === t.assumeFurther[Alphanumeric]

  inline def d: Double :| Greater[0] = 1.0
  inline def e: Double :| Greater[0] = 42.0

  val x = d === e
  val y = d < e

  inline val one = 1
  inline val two = 2
  inline val yup: true = one < two

/**
 */
object colors:

  final class Red
  final class Green
  final class Blue
  type White = Red | Green | Blue

  inline given [C <: White]: Constraint[Int, C] with
    override inline def test(value: Int): Boolean = true
    override inline def message: String = "some color C"

  val r: Int :| Red = 33
  val g: Int :| Green = 42
  val b: Int :| Blue = 93
  val w: Int :| White = 255
  val x: Int :| (Green | Blue) = 192
  val rgb: Int :| (Red | Green | Blue) = 255

  def shine[C <: White](c: Int :| C): Int :| C = c

  val zed: Int :| (Blue | Green) = shine(x)

  val zedr: Int :| White = shine(r)

  // problem arises here:
  // val zedrgb: Int :| (Red | Blue | Green) = shine(x)

// problem happens when we end up generating two implicit instances, one for each leg of the union type
// Found:    (io.deftrade.colors.x :
//   io.github.iltotore.iron.IronType[Int, io.deftrade.colors.Green |
//     io.deftrade.colors.Blue
//   ]
// )
// Required: io.github.iltotore.iron.IronType[Int, io.deftrade.colors.Red |
//   io.deftrade.colors.Blue
//  | io.deftrade.colors.Green]
// Note that implicit conversions cannot be applied because they are ambiguous;
// both io.github.iltotore.iron.Implication.given_==>_C1_C2 and io.github.iltotore.iron.Implication.given_==>_C1_C2 match type io.github.iltotore.iron.Implication[
//   (io.deftrade.colors.Green | io.deftrade.colors.Blue)
// , (io.deftrade.colors.Red | io.deftrade.colors.Blue | io.deftrade.colors.Green)]

  val zed2: Int :| White = shine(rgb)
  val zed3: Int :| (Green | (Red | Blue)) = shine(w)

  // derived type is non-redundant
  val omg: Int :| (Green | (Blue | Green)) = shine(x)
  val simplifies = shine(omg)
