/* Test the `v[min|max]{nm}{q}_f*' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

extern void abort ();

#define CHECK(T, N, R, E) \
  {\
    int i = 0;\
    for (; i < N; i++)\
      if (* (T *) &R[i] != * (T *) &E[i])\
	abort ();\
  }

int
main (int argc, char **argv)
{
  /* v{min|max}nm_f32 normal.  */
  float32x2_t f32x2_input1 = vdup_n_f32 (-1.0);
  float32x2_t f32x2_input2 = vdup_n_f32 (0.0);
  float32x2_t f32x2_exp_minnm  = vdup_n_f32 (-1.0);
  float32x2_t f32x2_exp_maxnm  = vdup_n_f32 (0.0);
  float32x2_t f32x2_ret_minnm  = vminnm_f32 (f32x2_input1, f32x2_input2);
  float32x2_t f32x2_ret_maxnm  = vmaxnm_f32 (f32x2_input1, f32x2_input2);

  CHECK (uint32_t, 2, f32x2_ret_minnm, f32x2_exp_minnm);
  CHECK (uint32_t, 2, f32x2_ret_maxnm, f32x2_exp_maxnm);

  /* v{min|max}nm_f32 NaN.  */
  f32x2_input1 = vdup_n_f32 (__builtin_nanf (""));
  f32x2_input2 = vdup_n_f32 (1.0);
  f32x2_exp_minnm  = vdup_n_f32 (1.0);
  f32x2_exp_maxnm  = vdup_n_f32 (1.0);
  f32x2_ret_minnm  = vminnm_f32 (f32x2_input1, f32x2_input2);
  f32x2_ret_maxnm  = vmaxnm_f32 (f32x2_input1, f32x2_input2);

  CHECK (uint32_t, 2, f32x2_ret_minnm, f32x2_exp_minnm);
  CHECK (uint32_t, 2, f32x2_ret_maxnm, f32x2_exp_maxnm);

  /* v{min|max}nmq_f32 normal.  */
  float32x4_t f32x4_input1 = vdupq_n_f32 (-1024.0);
  float32x4_t f32x4_input2 = vdupq_n_f32 (77.0);
  float32x4_t f32x4_exp_minnm  = vdupq_n_f32 (-1024.0);
  float32x4_t f32x4_exp_maxnm  = vdupq_n_f32 (77.0);
  float32x4_t f32x4_ret_minnm  = vminnmq_f32 (f32x4_input1, f32x4_input2);
  float32x4_t f32x4_ret_maxnm  = vmaxnmq_f32 (f32x4_input1, f32x4_input2);

  CHECK (uint32_t, 4, f32x4_ret_minnm, f32x4_exp_minnm);
  CHECK (uint32_t, 4, f32x4_ret_maxnm, f32x4_exp_maxnm);

  /* v{min|max}nmq_f32 NaN.  */
  f32x4_input1 = vdupq_n_f32 (-__builtin_nanf (""));
  f32x4_input2 = vdupq_n_f32 (-1.0);
  f32x4_exp_minnm  = vdupq_n_f32 (-1.0);
  f32x4_exp_maxnm  = vdupq_n_f32 (-1.0);
  f32x4_ret_minnm  = vminnmq_f32 (f32x4_input1, f32x4_input2);
  f32x4_ret_maxnm  = vmaxnmq_f32 (f32x4_input1, f32x4_input2);

  CHECK (uint32_t, 4, f32x4_ret_minnm, f32x4_exp_minnm);
  CHECK (uint32_t, 4, f32x4_ret_maxnm, f32x4_exp_maxnm);

  /* v{min|max}nm_f64 normal.  */
  float64x1_t f64x1_input1 = vdup_n_f64 (1.23);
  float64x1_t f64x1_input2 = vdup_n_f64 (4.56);
  float64x1_t f64x1_exp_minnm  = vdup_n_f64 (1.23);
  float64x1_t f64x1_exp_maxnm  = vdup_n_f64 (4.56);
  float64x1_t f64x1_ret_minnm  = vminnm_f64 (f64x1_input1, f64x1_input2);
  float64x1_t f64x1_ret_maxnm  = vmaxnm_f64 (f64x1_input1, f64x1_input2);
  CHECK (uint64_t, 1, f64x1_ret_minnm, f64x1_exp_minnm);
  CHECK (uint64_t, 1, f64x1_ret_maxnm, f64x1_exp_maxnm);

  /* v{min|max}_f64 normal.  */
  float64x1_t f64x1_exp_min  = vdup_n_f64 (1.23);
  float64x1_t f64x1_exp_max  = vdup_n_f64 (4.56);
  float64x1_t f64x1_ret_min  = vmin_f64 (f64x1_input1, f64x1_input2);
  float64x1_t f64x1_ret_max  = vmax_f64 (f64x1_input1, f64x1_input2);
  CHECK (uint64_t, 1, f64x1_ret_min, f64x1_exp_min);
  CHECK (uint64_t, 1, f64x1_ret_max, f64x1_exp_max);

  /* v{min|max}nmq_f64 normal.  */
  float64x2_t f64x2_input1 = vdupq_n_f64 (1.23);
  float64x2_t f64x2_input2 = vdupq_n_f64 (4.56);
  float64x2_t f64x2_exp_minnm  = vdupq_n_f64 (1.23);
  float64x2_t f64x2_exp_maxnm  = vdupq_n_f64 (4.56);
  float64x2_t f64x2_ret_minnm  = vminnmq_f64 (f64x2_input1, f64x2_input2);
  float64x2_t f64x2_ret_maxnm  = vmaxnmq_f64 (f64x2_input1, f64x2_input2);
  CHECK (uint64_t, 2, f64x2_ret_minnm, f64x2_exp_minnm);
  CHECK (uint64_t, 2, f64x2_ret_maxnm, f64x2_exp_maxnm);

  /* v{min|max}nm_f64 NaN.  */
  f64x1_input1 = vdup_n_f64 (-__builtin_nanf (""));
  f64x1_input2 = vdup_n_f64 (1.0);
  f64x1_exp_minnm  = vdup_n_f64 (1.0);
  f64x1_exp_maxnm  = vdup_n_f64 (1.0);
  f64x1_ret_minnm  = vminnm_f64 (f64x1_input1, f64x1_input2);
  f64x1_ret_maxnm  = vmaxnm_f64 (f64x1_input1, f64x1_input2);

  CHECK (uint64_t, 1, f64x1_ret_minnm, f64x1_exp_minnm);
  CHECK (uint64_t, 1, f64x1_ret_maxnm, f64x1_exp_maxnm);

  /* v{min|max}_f64 NaN.  */
  f64x1_input1 = vdup_n_f64 (-__builtin_nanf (""));
  f64x1_input2 = vdup_n_f64 (1.0);
  f64x1_exp_minnm  = vdup_n_f64 (-__builtin_nanf (""));
  f64x1_exp_maxnm  = vdup_n_f64 (-__builtin_nanf (""));
  f64x1_ret_minnm  = vmin_f64 (f64x1_input1, f64x1_input2);
  f64x1_ret_maxnm  = vmax_f64 (f64x1_input1, f64x1_input2);

  CHECK (uint64_t, 1, f64x1_ret_minnm, f64x1_exp_minnm);
  CHECK (uint64_t, 1, f64x1_ret_maxnm, f64x1_exp_maxnm);

  /* v{min|max}nmq_f64 NaN.  */
  f64x2_input1 = vdupq_n_f64 (-__builtin_nan (""));
  f64x2_input2 = vdupq_n_f64 (1.0);
  f64x2_exp_minnm  = vdupq_n_f64 (1.0);
  f64x2_exp_maxnm  = vdupq_n_f64 (1.0);
  f64x2_ret_minnm  = vminnmq_f64 (f64x2_input1, f64x2_input2);
  f64x2_ret_maxnm  = vmaxnmq_f64 (f64x2_input1, f64x2_input2);

  CHECK (uint64_t, 2, f64x2_ret_minnm, f64x2_exp_minnm);
  CHECK (uint64_t, 2, f64x2_ret_maxnm, f64x2_exp_maxnm);

  return 0;
}
