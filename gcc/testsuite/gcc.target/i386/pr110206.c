/* PR target/110206 */
/* { dg-do run } */
/* { dg-options "-Os -mavx512bw -mavx512vl" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */

#define AVX512BW
#define AVX512VL

#include "avx512f-check.h"

typedef unsigned char __attribute__((__vector_size__ (4))) U;
typedef unsigned char __attribute__((__vector_size__ (8))) V;
typedef unsigned short u16;

V g;

void
__attribute__((noinline))
foo (U u, u16 c, V *r)
{
  if (!c)
    abort ();
  V x = __builtin_shufflevector (u, (204 >> u), 7, 0, 5, 1, 3, 5, 0, 2);
  V y = __builtin_shufflevector (g, (V) { }, 7, 6, 6, 7, 2, 6, 3, 5);
  V z = __builtin_shufflevector (y, 204 * x, 3, 9, 8, 1, 4, 6, 14, 5);
  *r = z;
}

static void test_256 (void) { };

static void
test_128 (void)
{
  V r;
  foo ((U){4}, 5, &r);
  if (r[6] != 0x30)
    abort();
}
