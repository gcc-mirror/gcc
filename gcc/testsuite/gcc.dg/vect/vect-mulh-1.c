/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target int128 } */

#include "tree-vect.h"

#ifndef SIGNEDNESS
#define SIGNEDNESS signed
#endif

void __attribute__ ((noipa))
f (SIGNEDNESS long long *restrict a, SIGNEDNESS long long *restrict b,
   SIGNEDNESS long long *restrict c, __INTPTR_TYPE__ n)
{
  for (__INTPTR_TYPE__ i = 0; i < n; ++i)
    a[i] = ((SIGNEDNESS __int128) b[i] * c[i]) >> 64;
}

#define N 50
#define BASE1 0x1234567890abcdefULL
#define BASE2 0x0fedcba098765432ULL
#define CONST1 0x0123456789abcdefULL
#define CONST2 0x0f0e0d0c0b0a0908ULL

int
main (void)
{
  check_vect ();

  SIGNEDNESS long long a[N], b[N], c[N];
  /* Compute the inputs with wrapping unsigned arithmetic so that they cover
     the whole 64-bit range without overflowing a signed type.  */
  for (int i = 0; i < N; ++i)
    {
      b[i] = (SIGNEDNESS long long) (BASE1 + (unsigned long long) i * CONST1);
      c[i] = (SIGNEDNESS long long) (BASE2 + (unsigned long long) i * CONST2);
      asm volatile ("" ::: "memory");
    }
  b[0] = 0;
  c[0] = -1;
  b[1] = -1;
  c[1] = -1;
  f (a, b, c, N);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (a[i] != (SIGNEDNESS long long) (((SIGNEDNESS __int128) b[i] * c[i])
					>> 64))
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump {\.MULH} "vect" { target vect_mulh_di } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_mulh_di } } } */
