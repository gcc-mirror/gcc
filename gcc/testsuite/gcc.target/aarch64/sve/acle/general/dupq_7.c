/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svint8_t __attribute__ ((noipa))
make_s8 (int8_t x0, int8_t x1, int8_t x2, int8_t x3,
	 int8_t x4, int8_t x5, int8_t x6, int8_t x7,
	 int8_t x8, int8_t x9, int8_t xa, int8_t xb,
	 int8_t xc, int8_t xd, int8_t xe, int8_t xf)
{
  return svdupq_s8 (x0, x1, x2, x3, x4, x5, x6, x7,
		    x8, x9, xa, xb, xc, xd, xe, xf);
}

svint16_t __attribute__ ((noipa))
make_s16 (int16_t x0, int16_t x1, int16_t x2, int16_t x3,
	  int16_t x4, int16_t x5, int16_t x6, int16_t x7)
{
  return svdupq_s16 (x0, x1, x2, x3, x4, x5, x6, x7);
}

svint32_t __attribute__ ((noipa))
make_s32 (int32_t x0, int32_t x1, int32_t x2, int32_t x3)
{
  return svdupq_s32 (x0, x1, x2, x3);
}

svint64_t __attribute__ ((noipa))
make_s64 (int64_t x0, int64_t x1)
{
  return svdupq_s64 (x0, x1);
}

int8_t a[16] = { 1, -44, 91, -24, 101, -55, 77, 83,
		 -30, 69, 121, -128, -1, 13, 127, 26 };
int16_t b[8] = { -716, -10288, 30604, -19258, -9418, -10435, -16001, 7300 };
int32_t c[4] = { 1268374995, -1023602831, -891830021, -1793452959 };
int64_t d[2] = { 0x123456789abcdefLL, -0x123456789abcdefLL };

int
main ()
{
  svbool_t pg = svptrue_pat_b8 (SV_VL16);
  svint8_t s8 = make_s8 (a[0], a[1], a[2], a[3],
			 a[4], a[5], a[6], a[7],
			 a[8], a[9], a[10], a[11],
			 a[12], a[13], a[14], a[15]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, s8, svld1 (pg, a))))
    __builtin_abort ();

  svint16_t s16 = make_s16 (b[0], b[1], b[2], b[3],
			    b[4], b[5], b[6], b[7]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, s16, svld1 (pg, b))))
    __builtin_abort ();

  svint32_t s32 = make_s32 (c[0], c[1], c[2], c[3]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, s32, svld1 (pg, c))))
    __builtin_abort ();

  svint64_t s64 = make_s64 (d[0], d[1]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, s64, svld1 (pg, d))))
    __builtin_abort ();

  return 0;
}
