/* { dg-options "-Ofast" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>
#include <stddef.h>
#include <stdbool.h>

/*
** simple_gemm:
**	...
**	tbnz	[^\n]+
**	ld1	[^\n]+
**	fadd	[^\n]+
**	fadd	[^\n]+
**	st1	[^\n]+
**	ret
*/
void simple_gemm(
  float* restrict out,
  float const* restrict a,
  float const* restrict b,
  size_t k, bool zero_out
) {
  register float32x4x2_t o0;
  o0.val[0] = vdupq_n_f32(0.0f);
  o0.val[1] = vdupq_n_f32(0.0f);

  // begin dot
  {
    register float32x4_t a0;
    register float32x4x2_t b0;

    while (k >= 1) {
      b0 = vld1q_f32_x2(b);
      a0 = vdupq_n_f32(a[0]);

      o0.val[0] = vfmaq_f32(o0.val[0], a0, b0.val[0]);
      o0.val[1] = vfmaq_f32(o0.val[1], a0, b0.val[1]);

      b += 8;
      a += 1;
      k -= 1;
    }
  } // end dot

  // begin writeback
  {
    if (!zero_out) {
      register float32x4x2_t t0;
      t0 = vld1q_f32_x2(out);
      
      o0.val[0] = vaddq_f32(o0.val[0], t0.val[0]);
      o0.val[1] = vaddq_f32(o0.val[1], t0.val[1]);
    }

    // TODO: both clang and gcc generates redundant mov because of bad register allocation.
    vst1q_f32_x2(out, o0);
  } // end writeback
}
