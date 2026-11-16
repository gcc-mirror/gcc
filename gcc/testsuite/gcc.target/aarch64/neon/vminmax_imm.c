/* Regression test for PR128777 */
/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmin_f16_imm:
** movi	v([0-9]+)\.2s, #?0
** fmin	v0\.4h, (v0\.4h, v\1\.4h|v\1\.4h, v0\.4h)
** ret
*/
float16x4_t test_vmin_f16_imm (float16x4_t x) { return vmin_f16 (x, vdup_n_f16 (0.0)); }

/*
** test_vminq_f16_imm:
** fmov	v([0-9]+)\.8h, #?1\.0e\+0
** fmin	v0\.8h, (v0\.8h, v\1\.8h|v\1\.8h, v0\.8h)
** ret
*/
float16x8_t test_vminq_f16_imm (float16x8_t x) { return vminq_f16 (x, vdupq_n_f16 (1.0)); }

/*
** test_vmin_f32_imm:
** fmov	v([0-9]+)\.2s, #?2\.0e\+0
** fmin	v0\.2s, (v0\.2s, v\1\.2s|v\1\.2s, v0\.2s)
** ret
*/
float32x2_t test_vmin_f32_imm (float32x2_t x) { return vmin_f32 (x, vdup_n_f32 (2.0)); }

/*
** test_vminq_f32_imm:
** fmov	v([0-9]+)\.4s, #?3\.0e\+0
** fmin	v0\.4s, (v0\.4s, v\1\.4s|v\1\.4s, v0\.4s)
** ret
*/
float32x4_t test_vminq_f32_imm (float32x4_t x) { return vminq_f32 (x, vdupq_n_f32 (3.0)); }

/*
** test_vmin_f64_imm:
** fmov	d([0-9]+), #?4\.0e\+0
** fmin	d0, (d0, d\1|d\1, d0)
** ret
*/
float64x1_t test_vmin_f64_imm (float64x1_t x) { return vmin_f64 (x, vdup_n_f64 (4.0)); }

/*
** test_vminq_f64_imm:
** fmov	v([0-9]+)\.2d, #?5\.0e\+0
** fmin	v0\.2d, (v0\.2d, v\1\.2d|v\1\.2d, v0\.2d)
** ret
*/
float64x2_t test_vminq_f64_imm (float64x2_t x) { return vminq_f64 (x, vdupq_n_f64 (5.0)); }

/*
** test_vmax_f16_imm:
** fmov	v([0-9]+)\.4h, #?6\.0e\+0
** fmax	v0\.4h, (v0\.4h, v\1\.4h|v\1\.4h, v0\.4h)
** ret
*/
float16x4_t test_vmax_f16_imm (float16x4_t x) { return vmax_f16 (x, vdup_n_f16 (6.0)); }

/*
** test_vmaxq_f16_imm:
** fmov	v([0-9]+)\.8h, #?7\.0e\+0
** fmax	v0\.8h, (v0\.8h, v\1\.8h|v\1\.8h, v0\.8h)
** ret
*/
float16x8_t test_vmaxq_f16_imm (float16x8_t x) { return vmaxq_f16 (x, vdupq_n_f16 (7.0)); }

/*
** test_vmax_f32_imm:
** fmov	v([0-9]+)\.2s, #?8\.0e\+0
** fmax	v0\.2s, (v0\.2s, v\1\.2s|v\1\.2s, v0\.2s)
** ret
*/
float32x2_t test_vmax_f32_imm (float32x2_t x) { return vmax_f32 (x, vdup_n_f32 (8.0)); }

/*
** test_vmaxq_f32_imm:
** fmov	v([0-9]+)\.4s, #?9\.0e\+0
** fmax	v0\.4s, (v0\.4s, v\1\.4s|v\1\.4s, v0\.4s)
** ret
*/
float32x4_t test_vmaxq_f32_imm (float32x4_t x) { return vmaxq_f32 (x, vdupq_n_f32 (9.0)); }

/*
** test_vmax_f64_imm:
** fmov	d([0-9]+), #?1\.0e\+1
** fmax	d0, (d0, d\1|d\1, d0)
** ret
*/
float64x1_t test_vmax_f64_imm (float64x1_t x) { return vmax_f64 (x, vdup_n_f64 (10.0)); }

/*
** test_vmaxq_f64_imm:
** fmov	v([0-9]+)\.2d, #?1\.1e\+1
** fmax	v0\.2d, (v0\.2d, v\1\.2d|v\1\.2d, v0\.2d)
** ret
*/
float64x2_t test_vmaxq_f64_imm (float64x2_t x) { return vmaxq_f64 (x, vdupq_n_f64 (11.0)); }
