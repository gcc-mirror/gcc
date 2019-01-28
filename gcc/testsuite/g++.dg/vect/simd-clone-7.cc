// PR middle-end/67335
// { dg-do compile }
// { dg-additional-options "-fopenmp-simd" }

#pragma omp declare simd notinbranch uniform(y)
float
bar (float x, float *y, int)
{
  return y[0] + y[1] * x;
}
// { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target { { aarch64*-*-* } && lp64 } } .-4 }

