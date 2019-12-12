/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svuint8_t __attribute__ ((noipa))
make_u8 (uint8_t x0, uint8_t x1, uint8_t x2, uint8_t x3,
	 uint8_t x4, uint8_t x5, uint8_t x6, uint8_t x7,
	 uint8_t x8, uint8_t x9, uint8_t xa, uint8_t xb,
	 uint8_t xc, uint8_t xd, uint8_t xe, uint8_t xf)
{
  return svdupq_u8 (x0, x1, x2, x3, x4, x5, x6, x7,
		    x8, x9, xa, xb, xc, xd, xe, xf);
}

svuint16_t __attribute__ ((noipa))
make_u16 (uint16_t x0, uint16_t x1, uint16_t x2, uint16_t x3,
	  uint16_t x4, uint16_t x5, uint16_t x6, uint16_t x7)
{
  return svdupq_u16 (x0, x1, x2, x3, x4, x5, x6, x7);
}

svuint32_t __attribute__ ((noipa))
make_u32 (uint32_t x0, uint32_t x1, uint32_t x2, uint32_t x3)
{
  return svdupq_u32 (x0, x1, x2, x3);
}

svuint64_t __attribute__ ((noipa))
make_u64 (uint64_t x0, uint64_t x1)
{
  return svdupq_u64 (x0, x1);
}

uint8_t a[16] = { 1, 212, 91, 232, 101, 201, 77, 83,
		  226, 69, 121, 128, 255, 13, 127, 26 };
uint16_t b[8] = { 64820, 55248, 30604, 46278, 56118, 55101, 49535, 7300 };
uint32_t c[4] = { 1268374995, 3271364465, 3403137275, 2501514337 };
uint64_t d[2] = { 0x123456789abcdefULL, 0xfedcba9876543210ULL };

int
main ()
{
  svbool_t pg = svptrue_pat_b8 (SV_VL16);
  svuint8_t u8 = make_u8 (a[0], a[1], a[2], a[3],
			  a[4], a[5], a[6], a[7],
			  a[8], a[9], a[10], a[11],
			  a[12], a[13], a[14], a[15]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, u8, svld1 (pg, a))))
    __builtin_abort ();

  svuint16_t u16 = make_u16 (b[0], b[1], b[2], b[3],
			     b[4], b[5], b[6], b[7]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, u16, svld1 (pg, b))))
    __builtin_abort ();

  svuint32_t u32 = make_u32 (c[0], c[1], c[2], c[3]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, u32, svld1 (pg, c))))
    __builtin_abort ();

  svuint64_t u64 = make_u64 (d[0], d[1]);
  if (svptest_any (svptrue_b8 (), svcmpne (pg, u64, svld1 (pg, d))))
    __builtin_abort ();

  return 0;
}
