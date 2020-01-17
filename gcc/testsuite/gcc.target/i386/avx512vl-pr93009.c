/* PR target/93009 */
/* { dg-do run { target { avx512vl } } } */
/* { dg-options "-O2 -mavx512vl" } */

#define AVX512VL
#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256

#include "avx512f-check.h"

typedef double v2df __attribute__((vector_size (16)));

__attribute__((noipa)) v2df
foo (v2df x, v2df y, double *z)
{
  return x * y + (v2df) { z[0], z[0] };
}

__attribute__((noipa)) v2df
bar (v2df x, v2df y, double *z)
{
  return y * x + (v2df) { z[0], z[0] };
}

static void
test_256 (void)
{
}

static void
test_128 (void)
{
  double z = 5.0;
  v2df x = foo ((v2df) { 1.0, 2.0 }, (v2df) { 3.0, 4.0 }, &z);
  v2df y = bar ((v2df) { 6.0, 7.0 }, (v2df) { 8.0, 9.0 }, &z);
  if (x[0] != 8.0 || x[1] != 13.0 || y[0] != 53.0 || y[1] != 68.0)
    abort ();
}
