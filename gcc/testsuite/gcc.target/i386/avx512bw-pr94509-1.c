/* PR target/94509 */
/* { dg-do run { target avx512bw } } */
/* { dg-options "-O2 -mavx512bw" } */

#define AVX512BW
#include "avx512f-helper.h"

typedef unsigned short __attribute__ ((__vector_size__ (64))) V;

__attribute__((noipa)) V
foo (V x)
{
  return __builtin_shuffle (x, (V) { 0, 0, 0, 0, 0, 0, 0, 0,
				     15, 15, 15, 15, 15, 15, 15, 15,
				     0, 0, 0, 0, 0, 0, 0, 0,
				     15, 15, 15, 15, 15, 15, 15, 15 });
}

static void
TEST (void)
{
  V v = foo ((V) { 1, 2, 3, 4, 5, 6, 7, 8,
		   9, 10, 11, 12, 13, 14, 15, 16,
		   17, 18, 19, 20, 21, 22, 23, 24,
		   25, 26, 27, 28, 29, 30, 31, 32 });
  unsigned int i;
  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (v[i] != ((i & 8) ? 16 : 1))
      abort ();
}
