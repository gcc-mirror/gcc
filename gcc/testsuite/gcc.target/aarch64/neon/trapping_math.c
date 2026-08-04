/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-ftrapping-math" } */

/* These tests assert that floating point expressions are not removed, even if
   the result is unused, when `-ftrapping-math` is enabled. */

#include "arm_neon_test.h"

/*
** test_vadd_f16:
** fadd	v[0-9]+\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
void
test_vadd_f16 (float16x4_t a, float16x4_t b)
{
  vadd_f16 (a, b);
}

/*
** test_vadd_f32:
** fadd	v[0-9]+\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
void
test_vadd_f32 (float32x2_t a, float32x2_t b)
{
  vadd_f32 (a, b);
}

/*
** test_vadd_f64:
** fadd	d[0-9]+, (d0, d1|d1, d0)
** ret
*/
void
test_vadd_f64 (float64x1_t a, float64x1_t b)
{
  vadd_f64 (a, b);
}

/*
** test_vaddq_f16:
** fadd	v[0-9]+\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
void
test_vaddq_f16 (float16x8_t a, float16x8_t b)
{
  vaddq_f16 (a, b);
}

/*
** test_vaddq_f32:
** fadd	v[0-9]+\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
void
test_vaddq_f32 (float32x4_t a, float32x4_t b)
{
  vaddq_f32 (a, b);
}

/*
** test_vaddq_f64:
** fadd	v[0-9]+\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
void
test_vaddq_f64 (float64x2_t a, float64x2_t b)
{
  vaddq_f64 (a, b);
}

/*
** test_vabs_f16:
** fabs	v[0-9]+\.4h, v0\.4h
** ret
*/
void
test_vabs_f16 (float16x4_t a)
{
  vabs_f16 (a);
}

/*
** test_vabsq_f16:
** fabs	v[0-9]+\.8h, v0\.8h
** ret
*/
void
test_vabsq_f16 (float16x8_t a)
{
  vabsq_f16 (a);
}

/*
** test_vabs_f32:
** fabs	v[0-9]+\.2s, v0\.2s
** ret
*/
void
test_vabs_f32 (float32x2_t a)
{
  vabs_f32 (a);
}

/*
** test_vabsq_f32:
** fabs	v[0-9]+\.4s, v0\.4s
** ret
*/
void
test_vabsq_f32 (float32x4_t a)
{
  vabsq_f32 (a);
}

/*
** test_vabs_f64:
** fabs	d[0-9]+, d0
** ret
*/
void
test_vabs_f64 (float64x1_t a)
{
  vabs_f64 (a);
}

/*
** test_vabsq_f64:
** fabs	v[0-9]+\.2d, v0\.2d
** ret
*/
void
test_vabsq_f64 (float64x2_t a)
{
  vabsq_f64 (a);
}

/*
** test_vabd_f16:
** fabd	v[0-9]+\.4h, v0\.4h, v1\.4h
** ret
*/
void
test_vabd_f16 (float16x4_t a, float16x4_t b)
{
  vabd_f16 (a, b);
}

/*
** test_vabdq_f16:
** fabd	v[0-9]+\.8h, v0\.8h, v1\.8h
** ret
*/
void
test_vabdq_f16 (float16x8_t a, float16x8_t b)
{
  vabdq_f16 (a, b);
}

/*
** test_vabd_f32:
** fabd	v[0-9]+\.2s, v0\.2s, v1\.2s
** ret
*/
void
test_vabd_f32 (float32x2_t a, float32x2_t b)
{
  vabd_f32 (a, b);
}

/*
** test_vabdq_f32:
** fabd	v[0-9]+\.4s, v0\.4s, v1\.4s
** ret
*/
void
test_vabdq_f32 (float32x4_t a, float32x4_t b)
{
  vabdq_f32 (a, b);
}

/*
** test_vabd_f64:
** fabd	d[0-9]+, d0, d1
** ret
*/
void
test_vabd_f64 (float64x1_t a, float64x1_t b)
{
  vabd_f64 (a, b);
}

/*
** test_vabdq_f64:
** fabd	v[0-9]+\.2d, v0\.2d, v1\.2d
** ret
*/
void
test_vabdq_f64 (float64x2_t a, float64x2_t b)
{
  vabdq_f64 (a, b);
}

/*
** test_vabds_f32:
** fabd	s[0-9]+, s0, s1
** ret
*/
void
test_vabds_f32 (float32_t a, float32_t b)
{
  vabds_f32 (a, b);
}

/*
** test_vabdd_f64:
** fabd	d[0-9]+, d0, d1
** ret
*/
void
test_vabdd_f64 (float64_t a, float64_t b)
{
  vabdd_f64 (a, b);
}
