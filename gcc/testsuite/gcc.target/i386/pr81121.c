/* PR target/81121 */
/* { dg-do compile } */
/* { dg-options "-O0 -march=amdfam10 -mno-sse2" } */

void
foo (short *x, short *y)
{
  float a = 0;
  y[0] = x[0] * a;
}
