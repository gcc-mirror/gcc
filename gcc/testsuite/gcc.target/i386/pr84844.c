/* PR target/84844 */
/* { dg-do compile } */
/* { dg-options "-march=bdver1 -O2 -fschedule-insns -fselective-scheduling" } */

double
foo (int *x, int y, int z)
{
  *x = y;
  return z;
}
