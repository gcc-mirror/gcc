/* { dg-do compile } */
/* { dg-options "-O -ffast-math -ftrapping-math" } */

long
foo (int i)
{
  float x;
  x = i;
  return __builtin_lroundf (x);
}
