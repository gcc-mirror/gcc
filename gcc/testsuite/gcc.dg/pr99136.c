/* PR c/99136 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -w -fexcess-precision=standard" } */

void
foo (double x)
{
  return 1.0 / x;
}
