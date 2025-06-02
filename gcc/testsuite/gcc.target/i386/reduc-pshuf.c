/* { dg-do compile } */
/* { dg-options "-O3 -march=znver5 " } */

#define N 32
#define T short
T
foo (T *a)
{
  T sum = 0;
  for (int i = 0; i < N; i++)
    sum += a[i];
  return sum;
}

/* { dg-final { scan-assembler-times "vpsrl" 0 } } */
/* { dg-final { scan-assembler-times "vpshuf" 3 } } */
