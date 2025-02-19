/* { dg-do run } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-256 -O2 -fno-trapping-math" } */
/* { dg-require-effective-target avx10_2_256 } */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* Fast shift conversion here for convenience */
static __bf16
float_to_bf16 (float f)
{
  uint32_t float_bits;
  uint16_t bf16_bits;

  memcpy (&float_bits, &f, sizeof (float_bits));
  bf16_bits = (uint16_t) (float_bits >> 16);

  __bf16 bf;
  memcpy (&bf, &bf16_bits, sizeof (bf));
  return bf;
}

static float
bf16_to_float (__bf16 bf)
{
  uint32_t float_bits;
  uint16_t bf16_bits;

  memcpy (&bf16_bits, &bf, sizeof (bf16_bits));
  float_bits = ((uint32_t) bf16_bits) << 16;

  float f;
  memcpy (&f, &float_bits, sizeof (f));
  return f;
}

static void
test_eq (__bf16 a, __bf16 b)
{
  int result = (a == b);
  int expected = (bf16_to_float (a) == bf16_to_float (b));
  if (result != expected)
    abort ();
}

static void
test_ne (__bf16 a, __bf16 b)
{
  int result = (a != b);
  int expected = (bf16_to_float (a) != bf16_to_float (b));
  if (result != expected)
    abort ();
}

static void
test_lt (__bf16 a, __bf16 b)
{
  int result = (a < b);
  int expected = (bf16_to_float (a) < bf16_to_float (b));
  if (result != expected)
    abort ();
}

static void
test_le (__bf16 a, __bf16 b)
{
  int result = (a <= b);
  int expected = (bf16_to_float (a) <= bf16_to_float (b));
  if (result != expected)
    abort ();
}

static void
test_gt (__bf16 a, __bf16 b)
{
  int result = (a > b);
  int expected = (bf16_to_float (a) > bf16_to_float (b));
  if (result != expected)
    abort ();
}

static void
test_ge (__bf16 a, __bf16 b)
{
  int result = (a >= b);
  int expected = (bf16_to_float (a) >= bf16_to_float (b));
  if (result != expected)
    abort ();
}

int
main (void)
{
  if (!__builtin_cpu_supports ("avx10.2-256"))
    return 0;

  float test_values[] = {
    -10.0f, -1.0f, -0.5f, 0.0f, 0.5f, 1.0f, 10.0f, 100.0f, -100.0f
  };

  size_t num_values = sizeof (test_values) / sizeof (test_values[0]);

  for (size_t i = 0; i < num_values; i++)
      for (size_t j = 0; j < num_values; j++)
        {
          __bf16 a = float_to_bf16 (test_values[i]);
          __bf16 b = float_to_bf16 (test_values[j]);

          test_eq (a, b);
          test_ne (a, b);
          test_lt (a, b);
          test_le (a, b);
          test_gt (a, b);
          test_ge (a, b);
        }

  return 0;
}
