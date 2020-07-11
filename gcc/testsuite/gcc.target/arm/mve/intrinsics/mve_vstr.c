/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
void
foo (const int32_t *output_bias, int8_t *out, uint16_t num_ch)
{
  int32_t loop_count = num_ch;
  const int32_t *bias = output_bias;
  int i_loop_cnt;
  for (; i_loop_cnt < loop_count; out += 4, i_loop_cnt++)
   {
     int32x4_t out_0 = vldrwq_s32(bias);
     vstrbq_s32(out, out_0);
   }
}

void
foo1 (const int16_t *output_bias, int8_t *out, uint16_t num_ch)
{
  int32_t loop_count = num_ch;
  const int16_t *bias = output_bias;
  int i_loop_cnt;
  for (; i_loop_cnt < loop_count; out += 4, i_loop_cnt++)
   {
     int16x8_t out_0 = vldrhq_s16(bias);
     vstrbq_s16(out, out_0);
   }
}

void
foo2 (const int32_t *output_bias, int16_t *out, uint16_t num_ch)
{
  int32_t loop_count = num_ch;
  const int32_t *bias = output_bias;
  int i_loop_cnt;
  for (; i_loop_cnt < loop_count; out += 4, i_loop_cnt++)
   {
     int32x4_t out_0 = vldrwq_s32(bias);
     vstrhq_s32(out, out_0);
   }
}
