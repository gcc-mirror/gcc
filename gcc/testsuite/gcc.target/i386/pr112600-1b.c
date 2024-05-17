/* PR middle-end/112600 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

typedef unsigned short T;

void foo (T *out, T *x, T *y, int n)
{
  int i;

  for (i = 0; i < n; i++)
    out[i] = (x[i] + y[i]) | (-(T)((T)(x[i] + y[i]) < x[i]));
}

/* { dg-final { scan-assembler "paddusw" } } */
