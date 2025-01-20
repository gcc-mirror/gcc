/* { dg-do run { target aarch64_sve128_hw } } */
/* { dg-options "-O2 -fopenmp-simd -fno-trapping-math -msve-vector-bits=128 --param aarch64-autovec-preference=sve-only -fstack-protector-strong" } */

#pragma GCC target "+sve"

[[gnu::noipa]] float f(float *ptr, long n)
{
  float res = 0.0f;
#pragma omp simd reduction(+:res)
  for (long i = 0; i < n; ++i)
    if (ptr[i] >= 1.0f)
      res += ptr[i];
  return res;
}

[[gnu::noipa]] float g(float *ptr, long n)
{
  return f(ptr, n) + 1;
}

int
main ()
{
#define N 64 * 1024
  float data[N];
  for (long i = 0; i < N; ++i)
    data[i] = 1;
  if (g(data, N) != N + 1)
    __builtin_abort();
  return 0;
}
