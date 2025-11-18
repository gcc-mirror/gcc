/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd -mavx512vl" } */

#pragma omp declare simd
double __attribute__((noinline))
baz (double x)
{
  return x;
}

#pragma omp declare simd
double
foo (double d)
{
  return baz (d);
}

double __attribute__((noipa))
fn (double x)
{
  return foo (x);
}
