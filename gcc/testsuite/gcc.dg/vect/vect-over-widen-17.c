/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#define N 1024

/* This should not be treated as an over-widening pattern, even though
   "(b[i] & 0xef) | 0x80)" could be done in unsigned chars.  */

void __attribute__ ((noipa))
f (unsigned short *restrict a, unsigned short *restrict b)
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

  unsigned short a[N], b[N];
  for (int i = 0; i < N; ++i)
    {
      a[i] = i;
      b[i] = i * 3;
      asm volatile ("" ::: "memory");
    }
  f (a, b);
  for (int i = 0; i < N; ++i)
    if (a[i] != ((((i * 3) & 0xef) | 0x80) + (i << 4)))
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not {vect_recog_over_widening_pattern: detected} "vect" } } */
/* On Power, if there is no vect_hw_misalign support, unaligned vector access
   adopts realign_load scheme.  It requires rs6000_builtin_mask_for_load to
   generate mask whose return type is vector char.  */
/* { dg-final { scan-tree-dump-not {vector[^\n]*char} "vect" { target vect_hw_misalign } } } */
/* { dg-final { scan-tree-dump-not {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
