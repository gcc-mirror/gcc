/* PR target/95255 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4.1 -mfpmath=both" } */

double foo (double x)
{
  return __builtin_roundeven (x);
}
