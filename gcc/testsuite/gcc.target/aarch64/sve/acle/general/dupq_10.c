/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t __attribute__ ((noipa))
make_b8 (int8_t x0, int8_t x1, int8_t x2, int8_t x3,
	 int8_t x4, int8_t x5, int8_t x6, int8_t x7,
	 int8_t x8, int8_t x9, int8_t xa, int8_t xb,
	 int8_t xc, int8_t xd, int8_t xe, int8_t xf)
{
  return svdupq_b8 (x0, x1, x2, x3, x4, x5, x6, x7,
		    x8, x9, xa, xb, xc, xd, xe, xf);
}

svbool_t __attribute__ ((noipa))
make_b16 (int16_t x0, int16_t x1, int16_t x2, int16_t x3,
	  int16_t x4, int16_t x5, int16_t x6, int16_t x7)
{
  return svdupq_b16 (x0, x1, x2, x3, x4, x5, x6, x7);
}

svbool_t __attribute__ ((noipa))
make_b32 (int32_t x0, int32_t x1, int32_t x2, int32_t x3)
{
  return svdupq_b32 (x0, x1, x2, x3);
}

svbool_t __attribute__ ((noipa))
make_b64 (int64_t x0, int64_t x1)
{
  return svdupq_b64 (x0, x1);
}

int8_t a[16] = { 1, 0, 0, -3, 0, 9, 11, 0, 0, 1, 0, -4, 9, 9, 0, 0 };

int
main ()
{
  svbool_t pg = svptrue_pat_b8 (SV_VL16);
  svbool_t b8 = make_b8 (a[0], a[1], a[2], a[3],
			 a[4], a[5], a[6], a[7],
			 a[8], a[9], a[10], a[11],
			 a[12], a[13], a[14], a[15]);
  if (svptest_any (svptrue_b8 (),
		   sveor_z (pg, b8, svcmpne (pg, svld1 (pg, a), 0))))
    __builtin_abort ();

  svbool_t b16 = make_b16 (a[0], a[1], a[2], a[3],
			   a[4], a[5], a[6], a[7]);
  if (svptest_any (svptrue_b16 (),
		   sveor_z (pg, b16, svcmpne (pg, svld1sb_u16 (pg, a), 0))))
    __builtin_abort ();

  svbool_t b32 = make_b32 (a[0], a[1], a[2], a[3]);
  if (svptest_any (svptrue_b32 (),
		   sveor_z (pg, b32, svcmpne (pg, svld1sb_u32 (pg, a), 0))))
    __builtin_abort ();

  svbool_t b64 = make_b64 (a[0], a[1]);
  if (svptest_any (svptrue_b64 (),
		   sveor_z (pg, b64, svcmpne (pg, svld1sb_u64 (pg, a), 0))))
    __builtin_abort ();

  return 0;
}
