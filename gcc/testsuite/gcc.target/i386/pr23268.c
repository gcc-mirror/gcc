/* PR target/23268 */
/* Testcase reduced by Andrew Pinski */
/* { dg-do compile } */
/* { dg-options "-O1 -ffast-math" } */

int
f (float x)
{
  int a, b;
  a = __builtin_log (2.f);
  b = __builtin_lrint (x);
  return (a + b);
}
