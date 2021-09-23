/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-vmovw-4a.c"

__m512i
__attribute__ ((noinline,noclone))
foo3 (__m512i x)
{
  return foo1 (((__v32hi) x)[0]);
}

static void
do_test (void)
{
  short x = 25;
  union512i_w u = { -1, -1, -1, -1, -1, -1, -1, -1 };
  union512i_w exp = { 0, 0, 0, 0, 0, 0, 0, 0 };

  __m512i v;
  union512i_w a;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo1 (x);
  a.x = v;
  if (check_union512i_w (a, exp.a))
    abort ();

  x = 55;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo2 (&x);
  a.x = v;
  if (check_union512i_w (a, exp.a))
    abort ();

  x = 33;
  u.a[0] = x;
  exp.a[0] = x;
  memset (&v, -1, sizeof (v));
  v = foo3 (u.x);
  a.x = v;
  if (check_union512i_w (a, exp.a))
    abort ();
}
