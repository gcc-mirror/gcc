/* PR target/86036 */
/* { dg-do run } */
/* { dg-options "-O -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

typedef unsigned short V __attribute__ ((vector_size (64)));

__attribute__((noipa)) V
foo (V a)
{
  return a >= 3;
}

__attribute__((noipa)) V
bar (V a)
{
  return a != 0;
}

__attribute__((noipa)) V
baz (V a)
{
  return a == 0;
}

void
TEST (void)
{
  V a = (V) { 3, 17, 2, 0, 9, 1, 2, 3, 0, 0, 0, 3, 3, 3, 3, 3,
	      9, 16387, 9, 3, 3, 0, 0, 3, 3, 3, 0, 0, 0, 0, 3, 3 };
  V b = foo (a);
  V c = (V) { -1, -1, 0, 0, -1, 0, 0, -1, 0, 0, 0, -1, -1, -1, -1, -1,
	      -1, -1, -1, -1, -1, 0, 0, -1, -1, -1, 0, 0, 0, 0, -1, -1 };
  if (__builtin_memcmp (&b, &c, sizeof (b)))
    abort ();
  V d = bar (a);
  V e = (V) { -1, -1, -1, 0, -1, -1, -1, -1, 0, 0, 0, -1, -1, -1, -1, -1,
	      -1, -1, -1, -1, -1, 0, 0, -1, -1, -1, 0, 0, 0, 0, -1, -1 };
  if (__builtin_memcmp (&d, &e, sizeof (d)))
    abort ();
  V f = baz (a);
  V g = ~e;
  if (__builtin_memcmp (&f, &g, sizeof (f)))
    abort ();
}
