/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-vmovw-3a.c"

__m256i
__attribute__ ((noinline,noclone))
foo3 (__m256i x)
{
  return foo1 (((__v16hi) x)[0]);
}

static void
do_test (void)
{
  short x;
  union256i_w u = { -1, -1, -1, -1 };
  union256i_w exp = { 0, 0, 0, 0 };

  __m256i v;
  union256i_w a;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo1 (x);
  a.x = v;
  if (check_union256i_w (a, exp.a))
    abort ();

  x = 33;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo2 (&x);
  a.x = v;
  if (check_union256i_w (a, exp.a))
    abort ();

  x = -23;
  u.a[0] = x;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo3 (u.x);
  a.x = v;
  if (check_union256i_w (a, exp.a))
    abort ();
}
