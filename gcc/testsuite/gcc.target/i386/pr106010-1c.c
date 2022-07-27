/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-vectorize -fvect-cost-model=unlimited -mprefer-vector-width=256 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) _Float16>} 2 "vect" } } */
/* { dg-require-effective-target avx512fp16 } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"

#define N 10000

void
__attribute__((noipa))
foo_ph (_Complex _Float16* a, _Complex _Float16* b)
{
  for (int i = 0; i != N; i++)
    a[i] = b[i];
}

static void
do_test (void)
{
  _Complex _Float16* ph_src = (_Complex _Float16*) malloc (2 * N * sizeof (_Float16));
  _Complex _Float16* ph_dst = (_Complex _Float16*) malloc (2 * N * sizeof (_Float16));
  char* p_init = (char*) malloc (2 * N * sizeof (_Float16));

  __builtin_memset (ph_dst, 0, 2 * N * sizeof (_Float16));

  for (int i = 0; i != 2 * N * sizeof (_Float16); i++)
    p_init[i] = i;

  memcpy (ph_src, p_init, 2 * N * sizeof (_Float16));

  foo_ph (ph_dst, ph_src);
  if (__builtin_memcmp (ph_dst, ph_src, N * 2 * sizeof (_Float16)) != 0)
    __builtin_abort ();
}
