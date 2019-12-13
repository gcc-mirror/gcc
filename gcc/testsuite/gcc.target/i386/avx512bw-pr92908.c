/* PR target/92908 */
/* { dg-do run } */
/* { dg-options "-Og -fno-tree-fre -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

typedef unsigned short V __attribute__ ((__vector_size__ (64)));

V v;

void
TEST (void)
{
  int i;
  v = (V) v == v;
  for (i = 0; i < 32; i++)
    if (v[i] != 0xffff)
      abort ();
}
