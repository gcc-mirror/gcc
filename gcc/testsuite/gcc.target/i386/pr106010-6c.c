/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256 -fdump-tree-slp-details" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1 \}} 2 "slp2" } }  */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 1 "slp2" } } */

#include <string.h>

static void do_test (void);
#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a, _Complex _Float16* __restrict b)
{
  a[0] = b[15];
  a[1] = b[14];
  a[2] = b[13];
  a[3] = b[12];
  a[4] = b[11];
  a[5] = b[10];
  a[6] = b[9];
  a[7] = b[8];
  a[8] = b[7];
  a[9] = b[6];
  a[10] = b[5];
  a[11] = b[4];
  a[12] = b[3];
  a[13] = b[2];
  a[14] = b[1];
  a[15] = b[0];
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
    p[i] = i;

  __builtin_memcpy (ph_src, p, 64);

  for (int i = 0; i != 4; i++)
    {
      q[i] = i + 60;
      q[i + 4] = i + 56;
      q[i + 8] = i + 52;
      q[i + 12] = i + 48;
      q[i + 16] = i + 44;
      q[i + 20] = i + 40;
      q[i + 24] = i + 36;
      q[i + 28] = i + 32;
      q[i + 32] = i + 28;
      q[i + 36] = i + 24;
      q[i + 40] = i + 20;
      q[i + 44] = i + 16;
      q[i + 48] = i + 12;
      q[i + 52] = i + 8;
      q[i + 56] = i + 4;
      q[i + 60] = i;
    }

  __builtin_memcpy (ph_exp, q, 64);

  foo_ph (ph_dst, ph_src);
  
  if (__builtin_memcmp (ph_dst, ph_exp, 64) != 0)
    __builtin_abort ();

  return;
}
