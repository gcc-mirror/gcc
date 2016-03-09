/* PR target/70086 */
/* { dg-do compile } */
/* { dg-options "-mtune=barcelona -mavx512vl -ffloat-store" } */

float
foo (float a, float b, double c, float d, double e, float f)
{
  e -= d;
  d *= e;
  return e + d;
}
