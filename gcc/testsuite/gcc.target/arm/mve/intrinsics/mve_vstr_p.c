/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
void
foo1 (int8_t *x, int32_t * i1)
{
  mve_pred16_t p;
  int32x4_t x_0;
  int32_t * bias1 = i1;
  for (;; x++)
  {
    x_0 = vldrwq_s32(bias1);
    vstrbq_p_s32(x, x_0, p);
  }
}
void
foo2 (int8_t *x, int16_t * i1)
{
  mve_pred16_t p;
  int16x8_t x_0;
  int16_t * bias1 = i1;
  for (;; x++)
  {
    x_0 = vldrhq_s16(bias1);
    vstrbq_p_s16(x, x_0, p);
  }
}

void
foo3 (int16_t *x, int32_t * i1)
{
  mve_pred16_t p;
  int32x4_t x_0;
  int32_t * bias1 = i1;
  for (;; x++)
  {
    x_0 = vldrwq_s32(bias1);
    vstrhq_p_s32(x, x_0, p);
  }
}
