/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svfloat16_t __attribute__ ((noipa))
make_f16 (float16_t x0, float16_t x1, float16_t x2, float16_t x3,
	  float16_t x4, float16_t x5, float16_t x6, float16_t x7)
{
  return svdupq_f16 (x0, x1, x2, x3, x4, x5, x6, x7);
}

svfloat32_t __attribute__ ((noipa))
make_f32 (float32_t x0, float32_t x1, float32_t x2, float32_t x3)
{
  return svdupq_f32 (x0, x1, x2, x3);
}

svfloat64_t __attribute__ ((noipa))
make_f64 (float64_t x0, float64_t x1)
{
  return svdupq_f64 (x0, x1);
}

float16_t a[8] = { 1.0, -4.25, 9.75, 6.5, -2.125, 5.5, -3.75, 7.625 };
float32_t b[4] = { 1.0, -90.25, -11.75, 141.5 };
float64_t c[2] = { 9221.5, -4491.25 };

int
main ()
{
  svbool_t pg = svptrue_pat_b8 (SV_VL16);
  svfloat16_t f16 = make_f16 (a[0], a[1], a[2], a[3],
			      a[4], a[5], a[6], a[7]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, f16, svld1 (pg, a))))
    __builtin_abort ();

  svfloat32_t f32 = make_f32 (b[0], b[1], b[2], b[3]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, f32, svld1 (pg, b))))
    __builtin_abort ();

  svfloat64_t f64 = make_f64 (c[0], c[1]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, f64, svld1 (pg, c))))
    __builtin_abort ();

  return 0;
}
