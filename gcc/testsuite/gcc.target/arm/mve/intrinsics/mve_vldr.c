/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
void
foo (uint16_t row_x_col, int8_t *out)
{
  for (;;)
   {
     int32x4_t out_3;
     int8_t *rhs_0;
     int8_t *lhs_3;
     int i_row_x_col;
     for (;i_row_x_col < row_x_col; i_row_x_col++)
      {
	int32x4_t ker_0 = vldrbq_s32(rhs_0);
	int32x4_t ip_3 = vldrbq_s32(lhs_3);
	out_3 = vmulq_s32(ip_3, ker_0);
      }
     vstrbq_s32(out, out_3);
   }
}

void
foo1 (uint16_t row_x_col, int8_t *out)
{
  for (;;)
   {
     int16x8_t out_3;
     int8_t *rhs_0;
     int8_t *lhs_3;
     int i_row_x_col;
     for (; i_row_x_col < row_x_col; i_row_x_col++)
      {
	int16x8_t ker_0 = vldrbq_s16(rhs_0);
	int16x8_t ip_3 = vldrbq_s16(lhs_3);
	out_3 = vmulq_s16(ip_3, ker_0);
      }
     vstrbq_s16(out, out_3);
   }
}

void
foo2 (uint16_t row_x_col, int16_t *out)
{
  for (;;)
   {
     int32x4_t out_3;
     int16_t *rhs_0;
     int16_t *lhs_3;
     int i_row_x_col;
     for (; i_row_x_col < row_x_col; i_row_x_col++)
      {
	int32x4_t ker_0 = vldrhq_s32(rhs_0);
	int32x4_t ip_3 = vldrhq_s32(lhs_3);
	out_3 = vmulq_s32(ip_3, ker_0);
      }
     vstrhq_s32(out, out_3);
   }
}
