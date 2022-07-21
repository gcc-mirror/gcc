/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-slp-details -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 1 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) _Float16>} 4 "slp2" } } */

#include <string.h>

static void do_test (void);
#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a, _Complex _Float16* __restrict b)
{
  a[0] = b[8];
  a[1] = b[9];
  a[2] = b[10];
  a[3] = b[11];
  a[4] = b[12];
  a[5] = b[13];
  a[6] = b[14];
  a[7] = b[15];
  a[8] = b[0];
  a[9] = b[1];
  a[10] = b[2];
  a[11] = b[3];
  a[12] = b[4];
  a[13] = b[5];
  a[14] = b[6];
  a[15] = b[7];
}

void
do_test (void)
{
  _Complex _Float16* ph_src = (_Complex _Float16*) malloc (64);
  _Complex _Float16* ph_dst = (_Complex _Float16*) malloc (64);
  _Complex _Float16* ph_exp = (_Complex _Float16*) malloc (64);
  char* p = (char* ) malloc (64);
  char* q = (char* ) malloc (64);

  __builtin_memset (ph_dst, 0, 64);

  for (int i = 0; i != 64; i++)
    {
      p[i] = i;
      q[i] = (i + 32) % 64;
    }
  __builtin_memcpy (ph_src, p, 64);

  __builtin_memcpy (ph_exp, q, 64);

  foo_ph (ph_dst, ph_src);

  if (__builtin_memcmp (ph_dst, ph_exp, 64) != 0)
    __builtin_abort ();

  return;
}
