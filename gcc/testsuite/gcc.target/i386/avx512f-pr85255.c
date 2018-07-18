/* PR target/85255 */
/* { dg-do run { target { avx512f } } } */
/* { dg-options "-O2 -fno-tree-fre -mavx512f" } */

#include "avx512f-check.h"

typedef short V __attribute__ ((vector_size (64)));

V
foo (V v)
{
  v[v[1]] = 0;
  return v;
}

static void
avx512f_test (void)
{
  V v = foo ((V) { 1 });
  for (unsigned i = 0; i < 32; i++)
    if (v[i] != 0)
      __builtin_abort ();
}
