/* PR tree-optimization/70509 */
/* { dg-do run } */
/* { dg-options "-O1 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

typedef char V __attribute__ ((vector_size (64)));

int __attribute__ ((noinline, noclone))
foo (V u, V v)
{
  u /= v[0x20];
  return u[0];
}

void
TEST (void)
{
  int x = foo ((V) { 9 }, (V) { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
				3 });
  if (x != 3)
    abort ();
}
