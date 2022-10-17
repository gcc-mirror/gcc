/* { dg-do assemble } */
/* { dg-options "-O1 --save-temps" } */

#include <arm_neon.h>

int32x2_t foo1 (int32x2_t a)
{
  return vshr_n_s32 (vneg_s32 (a), 31);
}

int32x4_t foo2 (int32x4_t a)
{
  return vshrq_n_s32 (vnegq_s32 (a), 31);
}

int16x8_t foo3 (int16x8_t a)
{
  return vshrq_n_s16 (vnegq_s16 (a), 15);
}

int16x4_t foo4 (int16x4_t a)
{
  return vshr_n_s16 (vneg_s16 (a), 15);
}

int8x16_t foo5 (int8x16_t a)
{
  return vshrq_n_s8 (vnegq_s8 (a), 7);
}

int8x8_t foo6 (int8x8_t a)
{
  return vshr_n_s8 (vneg_s8 (a), 7);
}

/* { dg-final { scan-assembler-times {\tcmgt\t} 6 } } */
