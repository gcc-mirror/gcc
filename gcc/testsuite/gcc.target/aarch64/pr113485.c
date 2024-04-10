/* { dg-options "-O" } */

#include <arm_neon.h>

void test()
{
  while (1)
  {
    static const uint16_t jsimd_rgb_ycc_neon_consts[] = {19595, 0, 0, 0, 0, 0, 0, 0};
    uint16x8_t consts = vld1q_u16(jsimd_rgb_ycc_neon_consts);

    uint8_t tmp_buf[0];
    uint8x8x3_t input_pixels = vld3_u8(tmp_buf);
    uint16x8_t r = vmovl_u8(input_pixels.val[1]);
    uint32x4_t y_l = vmull_laneq_u16(vget_low_u16(r), consts, 0);

    uint32x4_t s = vdupq_n_u32(1);
    uint16x4_t a = vrshrn_n_u32(s, 16);
    uint16x4_t y = vrshrn_n_u32(y_l, 16);
    uint16x8_t ay = vcombine_u16(a, y);

    unsigned char ***out_buf;
    vst1_u8(out_buf[1][0], vmovn_u16(ay));
  }
}
