/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
void
foo (uint16_t row_len, const int32_t *bias, int8_t *out)
{
  int i_out_ch;
  for (;;)
   {
     int8_t *ip_c3;
     int32_t acc_3;
     int32_t row_loop_cnt = row_len;
     int32x4_t res = {acc_3};
     uint32x4_t scatter_offset;
     int i_row_loop;
     for (; i_row_loop < row_loop_cnt; i_row_loop++)
      {
	mve_pred16_t p;
	int16x8_t r0;
	int16x8_t c3 = vldrbq_z_s16(ip_c3, p);
	acc_3 = vmladavaq_p_s16(acc_3, r0, c3, p);
      }
     vstrbq_scatter_offset_s32(&out[i_out_ch], scatter_offset, res);
   }
}

void
foo1 (uint16_t row_len, const int32_t *bias, int8_t *out)
{
  int i_out_ch;
  for (;;)
   {
     int8_t *ip_c3;
     int32_t acc_3;
     int32_t row_loop_cnt = row_len;
     int i_row_loop;
     int32x4_t res = {acc_3};
     uint32x4_t scatter_offset;
     for (; i_row_loop < row_loop_cnt; i_row_loop++)
      {
	mve_pred16_t p;
	int32x4_t r0;
	int32x4_t c3 = vldrbq_z_s32(ip_c3, p);
	acc_3 = vmladavaq_p_s32(acc_3, r0, c3, p);
      }
     vstrbq_scatter_offset_s32(&out[i_out_ch], scatter_offset, res);
   }
}

void
foo2 (uint16_t row_len, const int32_t *bias, int8_t *out)
{
  int i_out_ch;
  for (;;)
   {
     int16_t *ip_c3;
     int32_t acc_3;
     int32_t row_loop_cnt = row_len;
     int i_row_loop;
     int32x4_t res = {acc_3};
     uint32x4_t scatter_offset;
     for (; i_row_loop < row_loop_cnt; i_row_loop++)
      {
	mve_pred16_t p;
	int32x4_t r0;
	int32x4_t c3 = vldrhq_z_s32(ip_c3, p);
	acc_3 = vmladavaq_p_s32(acc_3, r0, c3, p);
      }
     vstrbq_scatter_offset_s32(&out[i_out_ch], scatter_offset, res);
   }
}
