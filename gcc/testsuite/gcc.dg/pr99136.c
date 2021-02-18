/* PR c/99136 */
/* { dg-do compile } */
/* { dg-options "-w -fexcess-precision=standard" } */

void
foo (double x)
{
  return 1.0 / x;
}
