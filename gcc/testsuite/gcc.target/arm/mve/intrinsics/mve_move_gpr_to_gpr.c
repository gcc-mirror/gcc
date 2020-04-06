/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2 -mfloat-abi=softfp --save-temps" } */

#include "arm_mve.h"

extern int bar (float16x8_t, float16_t);

extern void foobar (float16_t);

int
foo (float16x8_t a, float16_t b)
{
  foobar (b);
  return bar (a, b);
}

