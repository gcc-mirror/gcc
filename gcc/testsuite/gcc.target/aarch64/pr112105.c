/* { dg-options "-O" } */

#include <arm_neon.h>
typedef struct {
  float re;
  float im;
} cmplx_f32_t;

void test2x2_f32(const cmplx_f32_t *p_src_a,
             const cmplx_f32_t *p_src_b,
             cmplx_f32_t *p_dst) {
  const float32_t *a_ptr = (const float32_t *)p_src_a;
  const float32_t *b_ptr = (const float32_t *)p_src_b;
  float32_t *out_ptr = (float32_t *)p_dst;

  float32x2x2_t a_col[2];
  float32x2x2_t b[2];
  float32x2x2_t result[2];

  a_col[0] = vld2_f32(a_ptr);
  b[0] = vld2_f32(b_ptr);

  result[0].val[0] = vmul_lane_f32(a_col[0].val[0], b[0].val[0], 0);
  result[0].val[1] = vmul_lane_f32(a_col[0].val[1], b[0].val[0], 0);

  vst2_f32(out_ptr, result[0]);
  out_ptr = out_ptr + 4;
}

/* { dg-final { scan-assembler-not {\tdup\t} } } */
/* { dg-final { scan-assembler-times {(?n)\tfmul\t.*v[0-9]+\.s\[0\]\n} 2 } } */
