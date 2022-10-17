/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mfpmath=sse" } */

#include <string.h>
#include <stdlib.h>
static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-11a.c"

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
  _Float16 x[32];
  _Float16 y[32];
  _Float16 res_add[32];
  _Float16 res_sub[32];
  _Float16 res_mul[32];
  _Float16 res_div[32];
  for (int i = 0 ; i != 32; i++)
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

  union512h u512 = { x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
      x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15],
      x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23],
      x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31] };
  union512h u512_1 = {y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7],
      y[8], y[9], y[10], y[11], y[12], y[13], y[14], y[15],
      y[16], y[17], y[18], y[19], y[20], y[21], y[22], y[23],
      y[24], y[25], y[26], y[27], y[28], y[29], y[30], y[31] };

  __m512h v512;
  union512h a512;

  memset (&v512, -1, sizeof (v512));
  v512 = vadd512 (u512.x, u512_1.x);
  a512.x = v512;
  if (check_union512h (a512, res_add))
    abort ();
  memset (&v512, -1, sizeof (v512));
  v512 = vsub512 (u512.x, u512_1.x);
  a512.x = v512;
  if (check_union512h (a512, res_sub))
    abort ();
  memset (&v512, -1, sizeof (v512));
  v512 = vmul512 (u512.x, u512_1.x);
  a512.x = v512;
  if (check_union512h (a512, res_mul))
    abort ();
  memset (&v512, -1, sizeof (v512));
  v512 = vdiv512 (u512.x, u512_1.x);
  a512.x = v512;
  if (check_union512h (a512, res_div))
    abort ();
}
