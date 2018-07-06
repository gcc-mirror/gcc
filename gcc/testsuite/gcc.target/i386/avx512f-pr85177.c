/* PR target/85177 */
/* { dg-do run { target { avx512f && int128 } } } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-sra -mavx512f -mno-avx512bw" } */

#include "avx512f-check.h"

typedef short U __attribute__ ((vector_size (64)));
typedef __int128 V __attribute__ ((vector_size (64)));

static inline __attribute__((always_inline)) U
foo (int i, U u)
{
  u[i & 1] = 1;
  return u;
}

__attribute__((noipa)) int
bar ()
{
  V x = (V) foo (0, (U) { });
  for (unsigned i = 0; i < 4; i++)
    if (x[i] != (i == 0)) __builtin_abort ();
  return 0;
}

static void
avx512f_test (void)
{
  bar ();
}
