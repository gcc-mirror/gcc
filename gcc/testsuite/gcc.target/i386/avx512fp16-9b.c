/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <string.h>

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16
#include "avx512-check.h"
#include "avx512fp16-9a.c"

union128h u128 = { ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16 };
union256h u256 = { ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16 };
union512h u512 = { ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16,
		   ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16, ESP_FLOAT16 };

static void
do_test (void)
{
  __m128h v128 = set1_128 (ESP_FLOAT16);
  __m256h v256 = set1_256 (ESP_FLOAT16);
  __m512h v512 = set1_512 (ESP_FLOAT16);
  union128h a128;
  union256h a256;
  union512h a512;

  a128.x = v128;
  if (check_union128h (a128, u128.a))
    abort ();

  a256.x = v256;
  if (check_union256h (a256, u256.a))
    abort ();

  a512.x = v512;
  if (check_union512h (a512, u512.a))
    abort ();
}
