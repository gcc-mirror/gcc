/* PR middle-end/89015 */
/* { dg-do compile } */

int
foo (int n, float *x, float *y)
{
  int i;
  int bar (void) { return i; }
#pragma omp teams distribute parallel for simd
  for (i = 0; i < n; i++)
    y[i] = x[i];
  return bar ();
}

int
baz (int n, float *x, float *y)
{
  int i;
  int qux (void) {
#pragma omp teams distribute parallel for simd
    for (i = 0; i < n; i++)
      y[i] = x[i];
  }
  return qux ();
}
