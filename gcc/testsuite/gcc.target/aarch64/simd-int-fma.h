#include <arm_neon.h>

#define DEF_FMA(TYPE) \
  TYPE __attribute__ ((noipa)) \
  add_##TYPE (TYPE a, TYPE b) \
  { \
    return a + b * 3; \
  } \
  TYPE __attribute__ ((noipa)) \
  sub_##TYPE (TYPE a, TYPE b) \
  { \
    return a - b * 3; \
  } \
  TYPE __attribute__ ((noipa)) \
  add_variable_##TYPE (TYPE a, TYPE b, TYPE c) \
  { \
    return a + b * c; \
  } \
  TYPE __attribute__ ((noipa)) \
  sub_variable_##TYPE (TYPE a, TYPE b, TYPE c) \
  { \
    return a - b * c; \
  }

#define ALL_TYPES(T) \
  T (int8x8_t) \
  T (uint8x8_t) \
  T (int8x16_t) \
  T (uint8x16_t) \
  T (int16x4_t) \
  T (uint16x4_t) \
  T (int16x8_t) \
  T (uint16x8_t) \
  T (int32x2_t) \
  T (uint32x2_t) \
  T (int32x4_t) \
  T (uint32x4_t)

ALL_TYPES (DEF_FMA)
