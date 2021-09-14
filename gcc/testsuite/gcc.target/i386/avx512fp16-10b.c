/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-10a.c"

union128h u128 = { ESP_FLOAT16, 0.0f, 0.0f, 0.0f,
		   0.0f, 0.0f, 0.0f, 0.0f };

static void
do_test (void)
{
  __m128h v128 = set_128 (ESP_FLOAT16);
  union128h a128;

  a128.x = v128;
  if (check_union128h (a128, u128.a))
    abort ();
}
