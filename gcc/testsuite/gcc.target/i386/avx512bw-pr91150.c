/* PR target/91150 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"

typedef unsigned char V __attribute__((vector_size (64)));

__attribute__((noipa)) void
foo (V *x, V *y, V *z)
{
  *x = __builtin_shuffle (*y, *z, (V) { 0, 1, 2, 3, 4, 5, 6, 7, 8,
					9, 10, 11, 12, 13, 14, 15,
					80, 81, 82, 83, 84, 85, 86, 87,
					88, 89, 90, 91, 92, 93, 94, 95,
					96, 97, 98, 99, 100, 101, 102, 103,
					104, 105, 106, 107, 108, 109, 110, 111,
				        112, 113, 114, 115, 116, 117, 118, 119,
					120, 121, 122, 123, 124, 125, 126, 127 });
}

static void
avx512bw_test (void)
{
  union U { unsigned char a[64]; V v; } a, b, c;
  int i;
  for (i = 0; i < 64; i++)
    {
      b.a[i] = i + 1;
      c.a[i] = i + 65;
    }
  foo (&a.v, &b.v, &c.v);
  for (i = 0; i < 64; i++)
    if (a.a[i] != (i < 16 ? i + 1 : i + 65))
      __builtin_abort ();
}
