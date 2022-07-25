/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) _Float16>} 1 "vect" } } */
/* { dg-require-effective-target avx512fp16 } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

#define N 10000

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1.0f16 + 2.0f16i;
}

static void
do_test (void)
{
  _Complex _Float16 ph_src = 1.0f16 + 2.0f16i;
  _Complex _Float16* ph_dst = (_Complex _Float16*) malloc (2 * N * sizeof (_Float16));

  __builtin_memset (ph_dst, 0, 2 * N * sizeof (_Float16));

  foo_ph (ph_dst);
  for (int i = 0; i != N; i++)
    {
      if (ph_dst[i] != ph_src)
	__builtin_abort ();
    }
}
