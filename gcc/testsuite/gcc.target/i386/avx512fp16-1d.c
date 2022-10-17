/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-1c.c"

static void
do_test (void)
{
  _Float16 x = 25.3;
  union128h u = { -1.2f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f };
  __m128h v;
  union128h a, b;
  v = foo1 (u.x, x);
  a.x = v;
  b = u;
  b.a[2] = x;
  if (check_union128h (a, b.a))
    abort ();
  x = 33.3;
  b = u;
  b.a[0] = x;
  v = foo2 (u.x, x);
  a.x = v;
  if (check_union128h (a, b.a))
    abort ();
}
