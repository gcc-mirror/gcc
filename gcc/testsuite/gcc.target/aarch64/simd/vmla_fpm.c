/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8fma" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vmlalbq_f16_fpm:
**	msr	fpmr, x0
**	fmlalb	v0.8h, v1.16b, v2.16b
**	ret
*/
float16x8_t
test_vmlalbq_f16_fpm (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalbq_f16_mf8_fpm (a, b, c, d);
}

/*
** test_vmlaltq_f16_fpm:
**	msr	fpmr, x0
**	fmlalt	v0.8h, v1.16b, v2.16b
**	ret
*/
float16x8_t
test_vmlaltq_f16_fpm (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlaltq_f16_mf8_fpm (a, b, c, d);
}

/*
** test_vmlallbbq_f32_fpm:
**	msr	fpmr, x0
**	fmlallbb	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
test_vmlallbbq_f32_fpm (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbbq_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vmlallbtq_f32_fpm:
**	msr	fpmr, x0
**	fmlallbt	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
test_vmlallbtq_f32_fpm (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbtq_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vmlalltbq_f32_fpm:
**	msr	fpmr, x0
**	fmlalltb	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
test_vmlalltbq_f32_fpm (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalltbq_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vmlallttq_f32_fpm:
**	msr	fpmr, x0
**	fmlalltt	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
test_vmlallttq_f32_fpm (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallttq_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vmlalbq_lane_f16_fpm_0:
**	msr	fpmr, x0
**	fmlalb	v0.8h, v1.16b, v2.b\[0\]
**	ret
*/
float16x8_t
test_vmlalbq_lane_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlalbq_lane_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlalbq_lane_f16_fpm_7:
**	msr	fpmr, x0
**	fmlalb	v0.8h, v1.16b, v2.b\[7\]
**	ret
*/
float16x8_t
test_vmlalbq_lane_f16_fpm_7 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlalbq_lane_f16_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlalbq_laneq_f16_fpm_0:
**	msr	fpmr, x0
**	fmlalb	v0.8h, v1.16b, v2.b\[0\]
**	ret
*/
float16x8_t
test_vmlalbq_laneq_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalbq_laneq_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlalbq_laneq_f16_fpm_15:
**	msr	fpmr, x0
**	fmlalb	v0.8h, v1.16b, v2.b\[15\]
**	ret
*/
float16x8_t
test_vmlalbq_laneq_f16_fpm_15 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalbq_laneq_f16_mf8_fpm (a, b, c, 15, d);
}

/*
** test_vmlaltq_lane_f16_fpm_0:
**	msr	fpmr, x0
**	fmlalt	v0.8h, v1.16b, v2.b\[0\]
**	ret
*/
float16x8_t
test_vmlaltq_lane_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlaltq_lane_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlaltq_lane_f16_fpm_7:
**	msr	fpmr, x0
**	fmlalt	v0.8h, v1.16b, v2.b\[7\]
**	ret
*/
float16x8_t
test_vmlaltq_lane_f16_fpm_7 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlaltq_lane_f16_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlaltq_laneq_f16_fpm_0:
**	msr	fpmr, x0
**	fmlalt	v0.8h, v1.16b, v2.b\[0\]
**	ret
*/
float16x8_t
test_vmlaltq_laneq_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlaltq_laneq_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlaltq_laneq_f16_fpm_15:
**	msr	fpmr, x0
**	fmlalt	v0.8h, v1.16b, v2.b\[15\]
**	ret
*/
float16x8_t
test_vmlaltq_laneq_f16_fpm_15 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlaltq_laneq_f16_mf8_fpm (a, b, c, 15, d);
}

/*
** test_vmlallbbq_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fmlallbb	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallbbq_lane_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallbbq_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallbbq_lane_f32_fpm_7:
**	msr	fpmr, x0
**	fmlallbb	v0.4s, v1.16b, v2.b\[7\]
**	ret
*/
float32x4_t
test_vmlallbbq_lane_f32_fpm_7 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallbbq_lane_f32_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlallbbq_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fmlallbb	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallbbq_laneq_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbbq_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallbbq_laneq_f32_fpm_15:
**	msr	fpmr, x0
**	fmlallbb	v0.4s, v1.16b, v2.b\[15\]
**	ret
*/
float32x4_t
test_vmlallbbq_laneq_f32_fpm_15 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbbq_laneq_f32_mf8_fpm (a, b, c, 15, d);
}

/*
** test_vmlallbtq_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fmlallbt	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallbtq_lane_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallbtq_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallbtq_lane_f32_fpm_7:
**	msr	fpmr, x0
**	fmlallbt	v0.4s, v1.16b, v2.b\[7\]
**	ret
*/
float32x4_t
test_vmlallbtq_lane_f32_fpm_7 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallbtq_lane_f32_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlallbtq_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fmlallbt	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallbtq_laneq_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbtq_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallbtq_laneq_f32_fpm_15:
**	msr	fpmr, x0
**	fmlallbt	v0.4s, v1.16b, v2.b\[15\]
**	ret
*/
float32x4_t
test_vmlallbtq_laneq_f32_fpm_15 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallbtq_laneq_f32_mf8_fpm (a, b, c, 15, d);
}

/*
** test_vmlalltbq_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fmlalltb	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlalltbq_lane_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlalltbq_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlalltbq_lane_f32_fpm_7:
**	msr	fpmr, x0
**	fmlalltb	v0.4s, v1.16b, v2.b\[7\]
**	ret
*/
float32x4_t
test_vmlalltbq_lane_f32_fpm_7 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlalltbq_lane_f32_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlalltbq_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fmlalltb	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlalltbq_laneq_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalltbq_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlalltbq_laneq_f32_fpm_15:
**	msr	fpmr, x0
**	fmlalltb	v0.4s, v1.16b, v2.b\[15\]
**	ret
*/
float32x4_t
test_vmlalltbq_laneq_f32_fpm_15 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlalltbq_laneq_f32_mf8_fpm (a, b, c, 15, d);
}

/*
** test_vmlallttq_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fmlalltt	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallttq_lane_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallttq_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallttq_lane_f32_fpm_7:
**	msr	fpmr, x0
**	fmlalltt	v0.4s, v1.16b, v2.b\[7\]
**	ret
*/
float32x4_t
test_vmlallttq_lane_f32_fpm_7 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vmlallttq_lane_f32_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vmlallttq_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fmlalltt	v0.4s, v1.16b, v2.b\[0\]
**	ret
*/
float32x4_t
test_vmlallttq_laneq_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallttq_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vmlallttq_laneq_f32_fpm_15:
**	msr	fpmr, x0
**	fmlalltt	v0.4s, v1.16b, v2.b\[15\]
**	ret
*/
float32x4_t
test_vmlallttq_laneq_f32_fpm_15 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vmlallttq_laneq_f32_mf8_fpm (a, b, c, 15, d);
}
