/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

#include <string.h>
#include <stdlib.h>
static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512vlfp16-11a.c"

/* Get random float16 between -50.x to 50.x.  */
_Float16
get_float16_noround()
{
  return ((int) (100.0 * rand ()/ (RAND_MAX + 1.0)) - 50)
    + 0.1f * (int) (10 * rand() / (RAND_MAX + 1.0));
}

static void
do_test (void)
{
  _Float16 x[16];
  _Float16 y[16];
  _Float16 res_add[16];
  _Float16 res_sub[16];
  _Float16 res_mul[16];
  _Float16 res_div[16];
  for (int i = 0 ; i != 16; i++)
    {
      x[i] = get_float16_noround ();
      y[i] = get_float16_noround ();
      if (y[i] == 0)
	y[i] = 1.0f;
      res_add[i] = x[i] + y[i];
      res_sub[i] = x[i] - y[i];
      res_mul[i] = x[i] * y[i];
      res_div[i] = x[i] / y[i];

    }

  union128h u128 = { x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7] };
  union128h u128_1 = { y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7] };
  union256h u256 = { x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
      x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15] };
  union256h u256_1 = { y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7],
      y[8], y[9], y[10], y[11], y[12], y[13], y[14], y[15]};

  __m128h v128;
  __m256h v256;
  union128h a128;
  union256h a256;

  memset (&v128, -1, sizeof (v128));
  v128 = vadd128 (u128.x, u128_1.x);
  a128.x = v128;
  if (check_union128h (a128, res_add))
    abort ();
  memset (&v128, -1, sizeof (v128));
  v128 = vsub128 (u128.x, u128_1.x);
  a128.x = v128;
  if (check_union128h (a128, res_sub))
    abort ();
  memset (&v128, -1, sizeof (v128));
  v128 = vmul128 (u128.x, u128_1.x);
  a128.x = v128;
  if (check_union128h (a128, res_mul))
    abort ();
  memset (&v128, -1, sizeof (v128));
  v128 = vdiv128 (u128.x, u128_1.x);
  a128.x = v128;
  if (check_union128h (a128, res_div))
    abort ();

  memset (&v256, -1, sizeof (v256));
  v256 = vadd256 (u256.x, u256_1.x);
  a256.x = v256;
  if (check_union256h (a256, res_add))
    abort ();
  memset (&v256, -1, sizeof (v256));
  v256 = vsub256 (u256.x, u256_1.x);
  a256.x = v256;
  if (check_union256h (a256, res_sub))
    abort ();
  memset (&v256, -1, sizeof (v256));
  v256 = vmul256 (u256.x, u256_1.x);
  a256.x = v256;
  if (check_union256h (a256, res_mul))
    abort ();
  memset (&v256, -1, sizeof (v256));
  v256 = vdiv256 (u256.x, u256_1.x);
  a256.x = v256;
  if (check_union256h (a256, res_div))
    abort ();
}
