/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfpu=auto -mfloat-abi=hard -mthumb --save-temps" } */

#include "arm_mve.h"

int8x16_t
foo8 ()
{
  int8x16_t b = {1, 2, 3, 4};
  return b;
}

int16x8_t
foo16 (int16x8_t value)
{
  int16x8_t b = {1, 2, 3};
  return b;
}

int32x4_t
foo32 (int32x4_t value)
{
  int32x4_t b = {1, 2};
  return b;
}

int64x2_t
foo64 (int64x2_t value)
{
  int64x2_t b = {1};
  return b;
}
