/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -O2 -mavx512f -masm=att" } */
/* { dg-final { scan-assembler "vpadd\[^\n\r]*%xmm" } } */
/* { dg-final { scan-assembler "vpadd\[^\n\r]*%ymm" } } */
/* { dg-final { scan-assembler "vpadd\[^\n\r]*%zmm" } } */

#define N 1024
int a[N];

void
f1 (void)
{
  int i;
  #pragma omp simd simdlen (4)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 1;
}

void
f2 (void)
{
  int i;
  #pragma omp simd simdlen (8)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 2;
}

void
f3 (void)
{
  int i;
  #pragma omp simd simdlen (16)
  for (i = 0; i < N; ++i)
    a[i] = a[i] + 3;
}
