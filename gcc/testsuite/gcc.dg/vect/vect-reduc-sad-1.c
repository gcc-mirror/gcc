/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 unsigned
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res, SIGNEDNESS_3 char *restrict a,
   SIGNEDNESS_3 char *restrict b)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      int av = a[i];
      int bv = b[i];
      short diff = av - bv;
      SIGNEDNESS_2 short abs = diff < 0 ? -diff : diff;
      res += abs;
    }
  return res;
}

#define BASE1 ((SIGNEDNESS_3 int) -1 < 0 ? -126 : 4)
#define BASE2 ((SIGNEDNESS_3 int) -1 < 0 ? 116 : 220)

int
main (void)
{
  check_vect ();

  SIGNEDNESS_3 char a[N], b[N];
  int expected = 0x12345;
  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE1 + i * 5;
      b[i] = BASE1 - i * 4;
      asm volatile ("" ::: "memory");
      int diff = a[i] - b[i];
      expected += (SIGNEDNESS_2 short) (diff < 0 ? -diff : diff);
    }
  if (f (0x12345, a, b) != expected)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_sad_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_sdot_qi } } } */
