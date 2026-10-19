/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

#include "simd-int-fma.h"
#include "simd-int-fma-shift.c"

static const uint32_t values[] = {
  0, 1, 0x7f, 0x80, 0xff, 0x7fff, 0x8000, 0xffff,
  0x7fffffff, 0x80000000, 0xffffffff, 0x55555555, 0xaaaaaaaa
};

#define CHECK(TYPE) \
  { \
    TYPE a, b, c; \
    unsigned int lanes = sizeof (a) / sizeof (a[0]); \
    for (unsigned int k = 0; k < lanes; ++k) \
      { \
	a[k] = values[(i + k) % count]; \
	b[k] = values[(j + 2 * k) % count]; \
	c[k] = values[(i + j + 3 * k) % count]; \
      } \
    TYPE add = add_##TYPE (a, b); \
    TYPE sub = sub_##TYPE (a, b); \
    TYPE add_variable = add_variable_##TYPE (a, b, c); \
    TYPE sub_variable = sub_variable_##TYPE (a, b, c); \
    for (unsigned int k = 0; k < lanes; ++k) \
      { \
	uint32_t sum = (uint32_t) a[k] + (uint32_t) b[k] * 3; \
	uint32_t diff = (uint32_t) a[k] - (uint32_t) b[k] * 3; \
	if (add[k] != (__typeof (a[k])) sum \
	    || sub[k] != (__typeof (a[k])) diff) \
	  __builtin_abort (); \
	uint32_t product = (uint32_t) b[k] * (uint32_t) c[k]; \
	sum = (uint32_t) a[k] + product; \
	diff = (uint32_t) a[k] - product; \
	if (add_variable[k] != (__typeof (a[k])) sum \
	    || sub_variable[k] != (__typeof (a[k])) diff) \
	  __builtin_abort (); \
      } \
  }

#define CHECK_SHIFT(TYPE, SUFFIX, FACTOR) \
  { \
    TYPE a, b; \
    TYPE factors = (TYPE) { 0 } + FACTOR; \
    unsigned int lanes = sizeof (a) / sizeof (a[0]); \
    for (unsigned int k = 0; k < lanes; ++k) \
      { \
	a[k] = values[(i + k) % count]; \
	b[k] = values[(j + 2 * k) % count]; \
      } \
    TYPE add = add_##SUFFIX (a, b); \
    TYPE sub = sub_##SUFFIX (a, b); \
    for (unsigned int k = 0; k < lanes; ++k) \
      { \
	uint32_t product = (uint32_t) b[k] * (uint32_t) factors[k]; \
	uint32_t sum = (uint32_t) a[k] + product; \
	uint32_t diff = (uint32_t) a[k] - product; \
	if (add[k] != (__typeof (a[k])) sum \
	    || sub[k] != (__typeof (a[k])) diff) \
	  __builtin_abort (); \
      } \
  }

int
main (void)
{
  unsigned int count = sizeof (values) / sizeof (values[0]);
  for (unsigned int i = 0; i < count; ++i)
    for (unsigned int j = 0; j < count; ++j)
      {
	ALL_TYPES (CHECK)
	CHECK_SHIFT (int16x8_t, s16, 8)
	CHECK_SHIFT (uint16x8_t, u16, 8)
	CHECK_SHIFT (int32x4_t, s32, -8)
	CHECK_SHIFT (uint32x4_t, u32, -8U)
	CHECK_SHIFT (uint16x8_t, positive, POSITIVE_POWERS)
	CHECK_SHIFT (int32x4_t, negative, NEGATIVE_POWERS)
      }
  return 0;
}
