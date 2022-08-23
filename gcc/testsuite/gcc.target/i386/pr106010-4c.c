/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -fdump-tree-slp-details -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 1 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) _Float16>} 1 "slp2" } } */

#include <string.h>

static void do_test (void);
#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a,
	_Complex _Float16 b1, _Complex _Float16 b2,
	_Complex _Float16 b3, _Complex _Float16 b4,
	_Complex _Float16 b5, _Complex _Float16 b6,
	_Complex _Float16 b7,_Complex _Float16 b8)
{
  a[0] = b1;
  a[1] = b2;
  a[2] = b3;
  a[3] = b4;
  a[4] = b5;
  a[5] = b6;
  a[6] = b7;
  a[7] = b8;
}

void
do_test (void)
{

  _Complex _Float16* ph_src = (_Complex _Float16*) malloc (32);
  _Complex _Float16* ph_dst = (_Complex _Float16*) malloc (32);

  char* p = (char* ) malloc (32);

  __builtin_memset (ph_dst, 0, 32);

  for (int i = 0; i != 32; i++)
    p[i] = i;

  __builtin_memcpy (ph_src, p, 32);

  foo_ph (ph_dst, ph_src[0], ph_src[1], ph_src[2], ph_src[3],
	  ph_src[4], ph_src[5], ph_src[6], ph_src[7]);

  if (__builtin_memcmp (ph_dst, ph_src, 32) != 0)
    __builtin_abort ();
  return;
}
