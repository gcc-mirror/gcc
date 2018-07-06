/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#define N 111

/* The addition should be narrowed to short.  */

void __attribute__ ((noipa))
f (unsigned int *restrict a, unsigned int *restrict b,
   unsigned short *restrict c, unsigned char *restrict d, unsigned int e)
{
  e &= 0xff;
  for (__INTPTR_TYPE__ i = 0; i < N; ++i)
    {
      unsigned int xor = d[i] ^ e;
      a[i] = c[i] | xor;
      b[i] = xor;
    }
}

int
main (void)
{
  check_vect ();

  unsigned int a[N], b[N];
  unsigned short c[N];
  unsigned char d[N];
  for (int i = 0; i < N; ++i)
    {
      c[i] = i * 11;
      d[i] = i * 2 + 3;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c, d, 0x73);
  for (int i = 0; i < N; ++i)
    if (b[i] != ((i * 2 + 3) ^ 0x73)
	|| a[i] != ((i * 11) | b[i]))
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {Splitting pattern statement} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \^} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \|} "vect" } } */
/* { dg-final { scan-tree-dump {demoting [^\n]* to [^\n]*char} "vect" } } */
/* { dg-final { scan-tree-dump {demoting [^\n]* to [^\n]*short} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
