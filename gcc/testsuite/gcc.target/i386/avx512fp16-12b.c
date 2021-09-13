/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-12a.c"

static void
do_test (void)
{
  _Float16 x = 0.1f;
  _Float16 y = -3.2f;
  _Float16 z;

  z = do_max (x, y);
  if (z != x)
    abort ();

  z = do_min (x, y);
  if (z != y)
    abort ();
}
