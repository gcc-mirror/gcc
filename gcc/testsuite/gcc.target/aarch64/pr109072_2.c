/* { dg-options "-O" } */

#pragma GCC target "arch=armv8.2-a+dotprod"

#include <arm_neon.h>

static inline uint32_t horizontal_add_uint32x4(const uint32x4_t a) {
  return vaddvq_u32(a);
}

static inline unsigned int sadwxh_avg_neon(const uint8_t *src_ptr,
                                           int src_stride,
                                           const uint8_t *ref_ptr,
                                           int ref_stride, int w, int h,
                                           const uint8_t *second_pred) {


  uint32x4_t sum[2] = { vdupq_n_u32(0), vdupq_n_u32(0) };

  int i = h;
  do {
    int j = 0;
    do {
      uint8x16_t s0, s1, r0, r1, p0, p1, avg0, avg1, diff0, diff1;

      s0 = vld1q_u8(src_ptr + j);
      r0 = vld1q_u8(ref_ptr + j);
      p0 = vld1q_u8(second_pred);
      avg0 = vrhaddq_u8(r0, p0);
      diff0 = vabdq_u8(s0, avg0);
      sum[0] = vdotq_u32(sum[0], diff0, vdupq_n_u8(1));

      s1 = vld1q_u8(src_ptr + j + 16);
      r1 = vld1q_u8(ref_ptr + j + 16);
      p1 = vld1q_u8(second_pred + 16);
      avg1 = vrhaddq_u8(r1, p1);
      diff1 = vabdq_u8(s1, avg1);
      sum[1] = vdotq_u32(sum[1], diff1, vdupq_n_u8(1));

      j += 32;
      second_pred += 32;
    } while (j < w);

    src_ptr += src_stride;
    ref_ptr += ref_stride;
  } while (--i != 0);

  return horizontal_add_uint32x4(vaddq_u32(sum[0], sum[1]));
}

static inline unsigned int sad32xh_avg_neon(const uint8_t *src_ptr,
                                            int src_stride,
                                            const uint8_t *ref_ptr,
                                            int ref_stride, int h,
                                            const uint8_t *second_pred) {
  return sadwxh_avg_neon(src_ptr, src_stride, ref_ptr, ref_stride, 32, h,
                         second_pred);
}

uint32_t vpx_sad32x16_avg_neon(const uint8_t *src, int src_stride, const uint8_t *ref, int ref_stride, const uint8_t *second_pred) { return sad32xh_avg_neon(src, src_stride, ref, ref_stride, (16), second_pred); }
