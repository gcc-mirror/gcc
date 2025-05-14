/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-do compile { target { loongarch_sx && {! loongarch_sx_hw } } } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -fvect-cost-model=dynamic" } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -fvect-cost-model=dynamic -mno-sse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-mlsx" { target { loongarch*-*-* } } } */

#include <stdlib.h>

#define N 128

#if __LONG_MAX__ == 2147483647
typedef short half_word;
#else
typedef int half_word;
#endif

int main_1 (int n, int *p)
{
  int i;
  half_word ib[N];
  half_word ia[N];
  int k;

  for (i = 0; i < N; i++)
    {
      ia[i] = n;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ia[i] != n)
        abort ();
    }

  k = *p;
  for (i = 0; i < N; i++)
    {
      ib[i] = k;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ib[i] != k)
        abort ();
    }

  return 0;
}

static volatile int n = 1;

int main (void)
{
  return main_1 (n + 2, (int *) &n);
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target { ! { avr-*-* msp430-*-* pru-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { target { ! { avr-*-* msp430-*-* pru-*-* } } } } } */
