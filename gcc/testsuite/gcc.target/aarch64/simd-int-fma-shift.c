/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#define DEF_SHIFT(TYPE, SUFFIX, FACTOR) \
  TYPE __attribute__ ((noipa)) add_##SUFFIX (TYPE a, TYPE b) \
  { \
    return a + b * FACTOR; \
  } \
  TYPE __attribute__ ((noipa)) sub_##SUFFIX (TYPE a, TYPE b) \
  { \
    return a - b * FACTOR; \
  }

DEF_SHIFT (int16x8_t, s16, 8)
DEF_SHIFT (uint16x8_t, u16, 8)
DEF_SHIFT (int32x4_t, s32, -8)
DEF_SHIFT (uint32x4_t, u32, -8U)

#define POSITIVE_POWERS \
  ((uint16x8_t) { 1, 2, 4, 8, 16, 32, 64, 32768 })
#define NEGATIVE_POWERS \
  ((int32x4_t) { -1, -2, -4, (-2147483647 - 1) })

DEF_SHIFT (uint16x8_t, positive, POSITIVE_POWERS)
DEF_SHIFT (int32x4_t, negative, NEGATIVE_POWERS)

/* { dg-final { scan-assembler-times {\tshl\tv} 8 } } */
/* { dg-final { scan-assembler-times {\tsshl\tv} 4 } } */
/* { dg-final { scan-assembler-not {\tmul\t|\tmla\t|\tmls\t} } } */
