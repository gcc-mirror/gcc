/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 signed
#endif

void __attribute__ ((noipa))
f (SIGNEDNESS_1 short *restrict a, SIGNEDNESS_2 char *restrict b,
   SIGNEDNESS_2 char *restrict c)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      int bv = (SIGNEDNESS_2 char) b[i];
      int cv = (SIGNEDNESS_2 char) c[i];
      a[i] = bv * cv;
    }
}

#define BASE ((SIGNEDNESS_2 int) -1 < 0 ? -126 : 4)
#define OFFSET 20

int
main (void)
{
  check_vect ();

  SIGNEDNESS_1 short a[N];
  SIGNEDNESS_2 char b[N], c[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE + i * 5;
      c[i] = BASE + OFFSET + i * 4;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (a[i] != (SIGNEDNESS_1 short) ((BASE + i * 5)
				      * (BASE + OFFSET + i * 4)))
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_widen_mult_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_widen_mult_qi_to_hi } } } */
