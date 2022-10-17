/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-2a.c"

static void
do_test (void)
{
  _Float16 x = 25.3;
  union256h u = { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		  0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
  __m256h v;
  union256h a;
  memset (&v, -1, sizeof (v));
  v = foo1 (x);
  a.x = v;
  if (check_union256h (a, u.a))
    abort ();
  x = 33.3;
  u.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo2 (&x);
  a.x = v;
  if (check_union256h (a, u.a))
    abort ();
}
