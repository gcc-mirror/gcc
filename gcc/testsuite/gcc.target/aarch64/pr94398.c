/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-vectorize -funsafe-math-optimizations -march=armv8.2-a+sve -mstrict-align" } */

float
foo(long n, float *x, int inc_x,
            float *y, int inc_y)
{
  float dot = 0.0;
  int ix = 0, iy = 0;

  if (n < 0) {
    return dot;
  }

  int i = 0;
  while (i < n) {
    dot += y[iy] * x[ix];
    ix  += inc_x;
    iy  += inc_y;
    i++;
  }

  return dot;
}
