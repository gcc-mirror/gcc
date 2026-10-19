/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include "insr-zero-gpr.c"

#define CHECK(BITS, COUNT) \
static __attribute__ ((noinline, noclone)) void \
check_##BITS (uint##BITS##_t value) \
{ \
  unsigned long count = COUNT; \
  uint##BITS##_t input[count], output[count]; \
  for (unsigned long i = 0; i < count; ++i) \
    input[i] = (uint##BITS##_t) (i * 13 + 7); \
  svst1_u##BITS (svptrue_b##BITS (), output, zero_##BITS (value)); \
  if (output[0] != value) \
    __builtin_abort (); \
  for (unsigned long i = 1; i < count; ++i) \
    if (output[i] != 0) \
      __builtin_abort (); \
  svuint##BITS##_t v = svld1_u##BITS (svptrue_b##BITS (), input); \
  svst1_u##BITS (svptrue_b##BITS (), output, nonzero_##BITS (v, value)); \
  if (output[0] != value) \
    __builtin_abort (); \
  for (unsigned long i = 1; i < count; ++i) \
    if (output[i] != input[i - 1]) \
      __builtin_abort (); \
}

CHECK (8, svcntb ())
CHECK (16, svcnth ())
CHECK (32, svcntw ())
CHECK (64, svcntd ())

#define CHECK_FLOAT(NAME, BITS, COUNT) \
static __attribute__ ((noinline, noclone)) void \
check_##NAME (uint##BITS##_t value) \
{ \
  unsigned long count = COUNT; \
  uint##BITS##_t output[count]; \
  svst1_u##BITS (svptrue_b##BITS (), output, \
		 svreinterpret_u##BITS (zero_##NAME (value))); \
  if (output[0] != value) \
    __builtin_abort (); \
  for (unsigned long i = 1; i < count; ++i) \
    if (output[i] != 0) \
      __builtin_abort (); \
}

CHECK_FLOAT (f16, 16, svcnth ())
CHECK_FLOAT (bf16, 16, svcnth ())
CHECK_FLOAT (f32, 32, svcntw ())
CHECK_FLOAT (f64, 64, svcntd ())

int
main (void)
{
  const uint64_t values[] = { 0, 1, 0x80, 0x8000, 0x80000000ULL,
			     0x8000000000000000ULL, UINT64_MAX,
			     0x123456789abcdef0ULL };
  for (unsigned int i = 0; i < sizeof (values) / sizeof (values[0]); ++i)
    {
      check_8 (values[i]);
      check_16 (values[i]);
      check_32 (values[i]);
      check_64 (values[i]);
      check_f16 (values[i]);
      check_bf16 (values[i]);
      check_f32 (values[i]);
      check_f64 (values[i]);
    }
  return 0;
}
