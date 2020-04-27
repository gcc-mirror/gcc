/* PR target/94780 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

_Atomic double x;

double
foo (void)
{
  double bar () { return x; }
  x /= 3;
  return bar ();
}
