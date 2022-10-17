/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include "tree-vect.h"

#define N 50

#ifndef SIGNEDNESS_1
#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 signed
#define SIGNEDNESS_4 unsigned
#endif

SIGNEDNESS_1 int __attribute__ ((noipa))
f (SIGNEDNESS_1 int res, SIGNEDNESS_3 char *restrict a,
   SIGNEDNESS_4 char *restrict b)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      int av = a[i];
      int bv = b[i];
      SIGNEDNESS_2 int mult = av * bv;
      res += mult;
    }
  return res;
}

#define BASE ((SIGNEDNESS_3 int) -1 < 0 ? -126 : 4)
#define OFFSET 20

int
main (void)
{
  check_vect ();

  SIGNEDNESS_3 char a[N];
  SIGNEDNESS_4 char b[N];
  SIGNEDNESS_1 int expected = 0x12345;
  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE + i * 5;
      b[i] = BASE + OFFSET + i * 4;
      asm volatile ("" ::: "memory");
      expected += (SIGNEDNESS_2 int) (a[i] * b[i]);
    }
  if (f (0x12345, a, b) != expected)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_sdot_qi } } } */
