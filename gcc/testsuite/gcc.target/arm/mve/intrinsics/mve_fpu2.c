/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_softfp_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-mfloat-abi=softfp" } */

#include "arm_mve.h"

int8x16_t
foo1 (int8x16_t value)
{
  int8x16_t b = -value;
  return b;
}
