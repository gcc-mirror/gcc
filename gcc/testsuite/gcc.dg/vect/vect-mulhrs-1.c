/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
#ifndef SIGNEDNESS
#define SIGNEDNESS signed
#endif
#ifndef BIAS
#define BIAS 0
#endif

#define HRS(x) ((((x) >> (15 - BIAS)) + BIAS) >> BIAS)

void __attribute__ ((noipa))
f (SIGNEDNESS short *restrict a, SIGNEDNESS short *restrict b,
   SIGNEDNESS short *restrict c, __INTPTR_TYPE__ n)
{
  for (__INTPTR_TYPE__ i = 0; i < n; ++i)
    a[i] = HRS((SIGNEDNESS int) b[i] * (SIGNEDNESS int) c[i]);
}

#define N 50
#define BASE1 ((SIGNEDNESS int) -1 < 0 ? -126 : 4)
#define BASE2 ((SIGNEDNESS int) -1 < 0 ? -101 : 26)
#define CONST1 0x01AB
#define CONST2 0x01CD

int
main (void)
{
  check_vect ();

  SIGNEDNESS short a[N], b[N], c[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE1 + i * CONST1;
      c[i] = BASE2 + i * CONST2;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c, N);
  for (int i = 0; i < N; ++i)
    if (a[i] != HRS(BASE1 * BASE2 + i * i * (CONST1 * CONST2)
		    + i * (BASE1 * CONST2 + BASE2 * CONST1)))
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_mulhs_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump {\.MULHS} "vect" { target vect_mulhrs_hi } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_mulhrs_hi } } } */
