/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#define N 1024

/* This should be treated as an over-widening pattern: we can truncate
   b to unsigned char after loading it and do all the computation in
   unsigned char.  */

void __attribute__ ((noipa))
f (unsigned char *restrict a, unsigned short *restrict b)
{
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      unsigned short foo = ((b[i] & 0xef) | 0x80) + (a[i] << 4);
      a[i] = foo;
    }
}

int
main (void)
{
  check_vect ();

  unsigned char a[N];
  unsigned short b[N];
  for (int i = 0; i < N; ++i)
    {
      a[i] = i;
      b[i] = i * 3;
      asm volatile ("" ::: "memory");
    }
  f (a, b);
  for (int i = 0; i < N; ++i)
    if (a[i] != (unsigned char) ((((i * 3) & 0xef) | 0x80) + (i << 4)))
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* &} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* |} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* <<} "vect" } } */
/* { dg-final { scan-tree-dump {vector[^\n]*char} "vect" } } */
/* { dg-final { scan-tree-dump-not {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
