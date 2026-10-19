/* PR target/127773 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler "vzeroupper.*call\[ \t\]+_?f\\M" } } */

typedef char v32qi __attribute__ ((vector_size (32)));

static __attribute__ ((noinline)) double
f (double *a, int n)
{
  double s = 0;
  for (int i = 0; i < n; i++)
    s += a[i] * a[i];
  return s;
}

__attribute__ ((target ("avx2"))) double
g (v32qi *p, double *a, int n)
{
  *p += *p;
  return f (a, n) + 1;
}
