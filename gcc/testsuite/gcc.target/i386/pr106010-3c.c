/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256 -fdump-tree-slp-details" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 1 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 2, 3, 0, 1, 8, 9, 6, 7, 14, 15, 12, 13, 4, 5, 10, 11 \}} 1 "slp2" } }  */

#include <string.h>

static void do_test (void);
#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a, _Complex _Float16* __restrict b)
{
  a[0] = b[1];
  a[1] = b[0];
  a[2] = b[4];
  a[3] = b[3];
  a[4] = b[7];
  a[5] = b[6];
  a[6] = b[2];
  a[7] = b[5];
}

void
do_test (void)
{
  _Complex _Float16* ph_src = (_Complex _Float16*) malloc (32);
  _Complex _Float16* ph_dst = (_Complex _Float16*) malloc (32);
  _Complex _Float16* ph_exp = (_Complex _Float16*) malloc (32);
  char* p = (char* ) malloc (32);
  char* q = (char* ) malloc (32);

  __builtin_memset (ph_dst, 0, 32);

  for (int i = 0; i != 32; i++)
    p[i] = i;
  __builtin_memcpy (ph_src, p, 32);

  for (int i = 0; i != 4; i++)
    {
      p[i] = i + 4;
      p[i + 4] = i;
      p[i + 8] = i + 16;
      p[i + 12] = i + 12;
      p[i + 16] = i + 28;
      p[i + 20] = i + 24;
      p[i + 24] = i + 8;
      p[i + 28] = i + 20;
      q[i] = i + 28;
      q[i + 4] = i + 24;
      q[i + 8] = i + 20;
      q[i + 12] = i + 16;
      q[i + 16] = i + 12;
      q[i + 20] = i + 8;
      q[i + 24] = i + 4;
      q[i + 28] = i;
    }
  __builtin_memcpy (ph_exp, p, 32);

  foo_ph (ph_dst, ph_src);
  if (__builtin_memcmp (ph_dst, ph_exp, 32) != 0)
    __builtin_abort ();

  return;
}
