/* PR106912 */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2 -fprofile-generate -ftree-vectorize" } */

__attribute__ ((__simd__))
__attribute__ ((__nothrow__ , __leaf__ , __const__, __noinline__))
double foo (double x);

void bar(double *f, int n)
{
  int i;
  for (i = 0; i < n; i++)
    f[i] = foo(f[i]);
}

double foo(double x)
{
  return x * x / 3.0;
}
