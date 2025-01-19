/* { dg-do assemble } */
/* { dg-additional-options "-O -std=gnu23 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** test_bsl1:
**	bsl	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_bsl1(uint8x8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vbsl_mf8(a, b, c);
}

/*
** test_bsl2:
**	bif	v0.8b, v2.8b, v1.8b
**	ret
*/
mfloat8x8_t test_bsl2(mfloat8x8_t a, uint8x8_t b, mfloat8x8_t c)
{
  return vbsl_mf8(b, a, c);
}

/*
** test_bsl3:
**	bit	v0.8b, v2.8b, v1.8b
**	ret
*/
mfloat8x8_t test_bsl3(mfloat8x8_t a, uint8x8_t b, mfloat8x8_t c)
{
  return vbsl_mf8(b, c, a);
}

/*
** test_bslq1:
**	bsl	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_bslq1(uint8x16_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vbslq_mf8(a, b, c);
}

/*
** test_bslq2:
**	bif	v0.16b, v2.16b, v1.16b
**	ret
*/
mfloat8x16_t test_bslq2(mfloat8x16_t a, uint8x16_t b, mfloat8x16_t c)
{
  return vbslq_mf8(b, a, c);
}

/*
** test_bslq3:
**	bit	v0.16b, v2.16b, v1.16b
**	ret
*/
mfloat8x16_t test_bslq3(mfloat8x16_t a, uint8x16_t b, mfloat8x16_t c)
{
  return vbslq_mf8(b, c, a);
}

/*
** test_combine1:
**	uzp1	v0.2d, v1.2d, v2.2d
**	ret
*/
mfloat8x16_t test_combine1(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vcombine_mf8(b, c);
}

/*
** test_copy_lane1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_copy_lane1(mfloat8x8_t a, mfloat8x8_t b)
{
  return vcopy_lane_mf8(a, 0, b, 0);
}

/*
** test_copy_lane2:
**	ins	v0.b\[0\], v1.b\[7\]
**	ret
*/
mfloat8x8_t test_copy_lane2(mfloat8x8_t a, mfloat8x8_t b)
{
  return vcopy_lane_mf8(a, 0, b, 7);
}

/*
** test_copy_lane3:
**	ins	v0.b\[7\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_copy_lane3(mfloat8x8_t a, mfloat8x8_t b)
{
  return vcopy_lane_mf8(a, 7, b, 0);
}

/*
** test_copy_lane4:
**	ins	v0.b\[5\], v1.b\[2\]
**	ret
*/
mfloat8x8_t test_copy_lane4(mfloat8x8_t a, mfloat8x8_t b)
{
  return vcopy_lane_mf8(a, 5, b, 2);
}

/*
** test_copy_laneq1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_copy_laneq1(mfloat8x8_t a, mfloat8x16_t b)
{
  return vcopy_laneq_mf8(a, 0, b, 0);
}

/*
** test_copy_laneq2:
**	ins	v0.b\[0\], v1.b\[15\]
**	ret
*/
mfloat8x8_t test_copy_laneq2(mfloat8x8_t a, mfloat8x16_t b)
{
  return vcopy_laneq_mf8(a, 0, b, 15);
}

/*
** test_copy_laneq3:
**	ins	v0.b\[7\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_copy_laneq3(mfloat8x8_t a, mfloat8x16_t b)
{
  return vcopy_laneq_mf8(a, 7, b, 0);
}

/*
** test_copy_laneq4:
**	ins	v0.b\[6\], v1.b\[13\]
**	ret
*/
mfloat8x8_t test_copy_laneq4(mfloat8x8_t a, mfloat8x16_t b)
{
  return vcopy_laneq_mf8(a, 6, b, 13);
}

/*
** test_copyq_lane1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_copyq_lane1(mfloat8x16_t a, mfloat8x8_t b)
{
  return vcopyq_lane_mf8(a, 0, b, 0);
}

/*
** test_copyq_lane2:
**	ins	v0.b\[0\], v1.b\[7\]
**	ret
*/
mfloat8x16_t test_copyq_lane2(mfloat8x16_t a, mfloat8x8_t b)
{
  return vcopyq_lane_mf8(a, 0, b, 7);
}

/*
** test_copyq_lane3:
**	ins	v0.b\[15\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_copyq_lane3(mfloat8x16_t a, mfloat8x8_t b)
{
  return vcopyq_lane_mf8(a, 15, b, 0);
}

/*
** test_copyq_lane4:
**	ins	v0.b\[11\], v1.b\[2\]
**	ret
*/
mfloat8x16_t test_copyq_lane4(mfloat8x16_t a, mfloat8x8_t b)
{
  return vcopyq_lane_mf8(a, 11, b, 2);
}

/*
** test_copyq_laneq1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_copyq_laneq1(mfloat8x16_t a, mfloat8x16_t b)
{
  return vcopyq_laneq_mf8(a, 0, b, 0);
}

/*
** test_copyq_laneq2:
**	ins	v0.b\[0\], v1.b\[15\]
**	ret
*/
mfloat8x16_t test_copyq_laneq2(mfloat8x16_t a, mfloat8x16_t b)
{
  return vcopyq_laneq_mf8(a, 0, b, 15);
}

/*
** test_copyq_laneq3:
**	ins	v0.b\[15\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_copyq_laneq3(mfloat8x16_t a, mfloat8x16_t b)
{
  return vcopyq_laneq_mf8(a, 15, b, 0);
}

/*
** test_copyq_laneq4:
**	ins	v0.b\[9\], v1.b\[13\]
**	ret
*/
mfloat8x16_t test_copyq_laneq4(mfloat8x16_t a, mfloat8x16_t b)
{
  return vcopyq_laneq_mf8(a, 9, b, 13);
}

/*
** test_create1:
**	fmov	d0, x0
**	ret
*/
mfloat8x8_t test_create1(uint64_t a)
{
  return vcreate_mf8(a);
}

/*
** test_create2:
**	movi	d0, #?0xffff
**	ret
*/
mfloat8x8_t test_create2()
{
  return vcreate_mf8(0xffff);
}

/*
** test_dup1:
**	dup	v0.8b, v1.b\[0\]
**	ret
*/
mfloat8x8_t test_dup1(mfloat8_t a, mfloat8_t b)
{
  return vdup_n_mf8(b);
}

/*
** test_dup2:
**	movi	v0.2s, #?0
**	ret
*/
mfloat8x8_t test_dup2()
{
  return vdup_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y);
}

/*
** test_dup3:
**	movi	v0.8b, #?0xf
**	ret
*/
mfloat8x8_t test_dup3()
{
  return vdup_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0x0f }).y);
}

/*
** test_dupq1:
**	dup	v0.16b, v1.b\[0\]
**	ret
*/
mfloat8x16_t test_dupq1(mfloat8_t a, mfloat8_t b)
{
  return vdupq_n_mf8(b);
}

/*
** test_dupq2:
**	movi	v0.4s, #?0
**	ret
*/
mfloat8x16_t test_dupq2()
{
  return vdupq_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y);
}

/*
** test_dupq3:
**	movi	v0.16b, #?0xf
**	ret
*/
mfloat8x16_t test_dupq3()
{
  return vdupq_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0x0f }).y);
}

/*
** test_dup_lane1:
**	dup	v0.8b, v1.b\[0\]
**	ret
*/
mfloat8x8_t test_dup_lane1(mfloat8_t a, mfloat8x8_t b)
{
  return vdup_lane_mf8(b, 0);
}

/*
** test_dup_lane2:
**	dup	v0.8b, v1.b\[7\]
**	ret
*/
mfloat8x8_t test_dup_lane2(mfloat8_t a, mfloat8x8_t b)
{
  return vdup_lane_mf8(b, 7);
}

/*
** test_dup_laneq1:
**	dup	v0.8b, v1.b\[0\]
**	ret
*/
mfloat8x8_t test_dup_laneq1(mfloat8_t a, mfloat8x16_t b)
{
  return vdup_laneq_mf8(b, 0);
}

/*
** test_dup_laneq2:
**	dup	v0.8b, v1.b\[15\]
**	ret
*/
mfloat8x8_t test_dup_laneq2(mfloat8_t a, mfloat8x16_t b)
{
  return vdup_laneq_mf8(b, 15);
}

/*
** test_dupq_lane1:
**	dup	v0.16b, v1.b\[0\]
**	ret
*/
mfloat8x16_t test_dupq_lane1(mfloat8_t a, mfloat8x8_t b)
{
  return vdupq_lane_mf8(b, 0);
}

/*
** test_dupq_lane2:
**	dup	v0.16b, v1.b\[7\]
**	ret
*/
mfloat8x16_t test_dupq_lane2(mfloat8_t a, mfloat8x8_t b)
{
  return vdupq_lane_mf8(b, 7);
}

/*
** test_dupq_laneq1:
**	dup	v0.16b, v1.b\[0\]
**	ret
*/
mfloat8x16_t test_dupq_laneq1(mfloat8_t a, mfloat8x16_t b)
{
  return vdupq_laneq_mf8(b, 0);
}

/*
** test_dupq_laneq2:
**	dup	v0.16b, v1.b\[15\]
**	ret
*/
mfloat8x16_t test_dupq_laneq2(mfloat8_t a, mfloat8x16_t b)
{
  return vdupq_laneq_mf8(b, 15);
}

/*
** test_dupb_lane1:
**	dup	b0, v1.b\[0\]
**	ret
*/
mfloat8_t test_dupb_lane1(mfloat8_t a, mfloat8x8_t b)
{
  return vdupb_lane_mf8(b, 0);
}

/*
** test_dupb_lane2:
**	dup	b0, v1.b\[7\]
**	ret
*/
mfloat8_t test_dupb_lane2(mfloat8_t a, mfloat8x8_t b)
{
  return vdupb_lane_mf8(b, 7);
}

/*
** test_dupb_laneq1:
**	dup	b0, v1.b\[0\]
**	ret
*/
mfloat8_t test_dupb_laneq1(mfloat8_t a, mfloat8x16_t b)
{
  return vdupb_laneq_mf8(b, 0);
}

/*
** test_dupb_laneq2:
**	dup	b0, v1.b\[15\]
**	ret
*/
mfloat8_t test_dupb_laneq2(mfloat8_t a, mfloat8x16_t b)
{
  return vdupb_laneq_mf8(b, 15);
}

/*
** test_ext1:
**	ext	v0.8b, v0.8b, v1.8b, #1
**	ret
*/
mfloat8x8_t test_ext1(mfloat8x8_t a, mfloat8x8_t b)
{
  return vext_mf8(a, b, 1);
}

/*
** test_ext2:
**	ext	v0.8b, v1.8b, v2.8b, #7
**	ret
*/
mfloat8x8_t test_ext2(mfloat8x8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vext_mf8(b, c, 7);
}

/*
** test_extq1:
**	ext	v0.16b, v0.16b, v1.16b, #1
**	ret
*/
mfloat8x16_t test_extq1(mfloat8x16_t a, mfloat8x16_t b)
{
  return vextq_mf8(a, b, 1);
}

/*
** test_extq2:
**	ext	v0.16b, v1.16b, v2.16b, #15
**	ret
*/
mfloat8x16_t test_extq2(mfloat8x16_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vextq_mf8(b, c, 15);
}

/*
** test_ld1:	{ target { le && lp64 } }
**	ldr	d0, \[x0\]
**	ret
*/
/*
** test_ld1:	{ target { be && lp64 } }
**	ld1	{v0.8b}, \[x0\]
**	ret
*/
mfloat8x8_t test_ld1(const mfloat8_t *ptr)
{
  return vld1_mf8(ptr);
}

/*
** test_ld1q:	{ target { le && lp64 } }
**	ldr	q0, \[x0\]
**	ret
*/
/*
** test_ld1q:	{ target { be && lp64 } }
**	ld1	{v0.16b}, \[x0\]
**	ret
*/
mfloat8x16_t test_ld1q(const mfloat8_t *ptr)
{
  return vld1q_mf8(ptr);
}

/*
** test_ld1_dup:	{ target lp64 }
**	ld1r	{v0.8b}, \[x0\]
**	ret
*/
mfloat8x8_t test_ld1_dup(const mfloat8_t *ptr)
{
  return vld1_dup_mf8(ptr);
}

/*
** test_ld1q_dup:	{ target lp64 }
**	ld1r	{v0.16b}, \[x0\]
**	ret
*/
mfloat8x16_t test_ld1q_dup(const mfloat8_t *ptr)
{
  return vld1q_dup_mf8(ptr);
}

/*
** test_ld1_lane1:	{ target lp64 }
**	ld1	{v0.b}\[0\], \[x0\]
**	ret
*/
mfloat8x8_t test_ld1_lane1(const mfloat8_t *ptr, mfloat8x8_t a)
{
  return vld1_lane_mf8(ptr, a, 0);
}

/*
** test_ld1_lane2:	{ target lp64 }
**	ld1	{v0.b}\[7\], \[x0\]
**	ret
*/
mfloat8x8_t test_ld1_lane2(const mfloat8_t *ptr, mfloat8x8_t a)
{
  return vld1_lane_mf8(ptr, a, 7);
}

/*
** test_ld1q_lane1:	{ target lp64 }
**	ld1	{v0.b}\[0\], \[x0\]
**	ret
*/
mfloat8x16_t test_ld1q_lane1(const mfloat8_t *ptr, mfloat8x16_t a)
{
  return vld1q_lane_mf8(ptr, a, 0);
}

/*
** test_ld1q_lane2:	{ target lp64 }
**	ld1	{v0.b}\[15\], \[x0\]
**	ret
*/
mfloat8x16_t test_ld1q_lane2(const mfloat8_t *ptr, mfloat8x16_t a)
{
  return vld1q_lane_mf8(ptr, a, 15);
}

/*
** test_ld1_x2:	{ target lp64 }
**	ld1	{v0.8b( - |, )v1.8b}, \[x0\]
**	ret
*/
mfloat8x8x2_t test_ld1_x2(const mfloat8_t *ptr)
{
  return vld1_mf8_x2(ptr);
}

/*
** test_ld1q_x2:	{ target lp64 }
**	ld1	{v0.16b( - |, )v1.16b}, \[x0\]
**	ret
*/
mfloat8x16x2_t test_ld1q_x2(const mfloat8_t *ptr)
{
  return vld1q_mf8_x2(ptr);
}

/*
** test_ld1_x3:	{ target lp64 }
**	ld1	{v0.8b - v2.8b}, \[x0\]
**	ret
*/
mfloat8x8x3_t test_ld1_x3(const mfloat8_t *ptr)
{
  return vld1_mf8_x3(ptr);
}

/*
** test_ld1q_x3:	{ target lp64 }
**	ld1	{v0.16b - v2.16b}, \[x0\]
**	ret
*/
mfloat8x16x3_t test_ld1q_x3(const mfloat8_t *ptr)
{
  return vld1q_mf8_x3(ptr);
}

/*
** test_ld1_x4:	{ target lp64 }
**	ld1	{v0.8b - v3.8b}, \[x0\]
**	ret
*/
mfloat8x8x4_t test_ld1_x4(const mfloat8_t *ptr)
{
  return vld1_mf8_x4(ptr);
}

/*
** test_ld1q_x4:	{ target lp64 }
**	ld1	{v0.16b - v3.16b}, \[x0\]
**	ret
*/
mfloat8x16x4_t test_ld1q_x4(const mfloat8_t *ptr)
{
  return vld1q_mf8_x4(ptr);
}

/*
** test_ld2:	{ target lp64 }
**	ld2	{v0.8b( - |, )v1.8b}, \[x0\]
**	ret
*/
mfloat8x8x2_t test_ld2(const mfloat8_t *ptr)
{
  return vld2_mf8(ptr);
}

/*
** test_ld2q:	{ target lp64 }
**	ld2	{v0.16b( - |, )v1.16b}, \[x0\]
**	ret
*/
mfloat8x16x2_t test_ld2q(const mfloat8_t *ptr)
{
  return vld2q_mf8(ptr);
}

/*
** test_ld2_dup:	{ target lp64 }
**	ld2r	{v0.8b( - |, )v1.8b}, \[x0\]
**	ret
*/
mfloat8x8x2_t test_ld2_dup(const mfloat8_t *ptr)
{
  return vld2_dup_mf8(ptr);
}

/*
** test_ld2q_dup:	{ target lp64 }
**	ld2r	{v0.16b( - |, )v1.16b}, \[x0\]
**	ret
*/
mfloat8x16x2_t test_ld2q_dup(const mfloat8_t *ptr)
{
  return vld2q_dup_mf8(ptr);
}

/*
** test_ld2_lane1:	{ target lp64 }
**	ld2	{v0.b( - |, )v1.b}\[0\], \[x0\]
**	ret
*/
mfloat8x8x2_t test_ld2_lane1(const mfloat8_t *ptr, mfloat8x8x2_t a)
{
  return vld2_lane_mf8(ptr, a, 0);
}

/*
** test_ld2_lane2:	{ target lp64 }
**	ld2	{v0.b( - |, )v1.b}\[7\], \[x0\]
**	ret
*/
mfloat8x8x2_t test_ld2_lane2(const mfloat8_t *ptr, mfloat8x8x2_t a)
{
  return vld2_lane_mf8(ptr, a, 7);
}

/*
** test_ld2q_lane1:	{ target lp64 }
**	ld2	{v0.b( - |, )v1.b}\[0\], \[x0\]
**	ret
*/
mfloat8x16x2_t test_ld2q_lane1(const mfloat8_t *ptr, mfloat8x16x2_t a)
{
  return vld2q_lane_mf8(ptr, a, 0);
}

/*
** test_ld2q_lane2:	{ target lp64 }
**	ld2	{v0.b( - |, )v1.b}\[15\], \[x0\]
**	ret
*/
mfloat8x16x2_t test_ld2q_lane2(const mfloat8_t *ptr, mfloat8x16x2_t a)
{
  return vld2q_lane_mf8(ptr, a, 15);
}

/*
** test_ld3:	{ target lp64 }
**	ld3	{v0.8b - v2.8b}, \[x0\]
**	ret
*/
mfloat8x8x3_t test_ld3(const mfloat8_t *ptr)
{
  return vld3_mf8(ptr);
}

/*
** test_ld3q:	{ target lp64 }
**	ld3	{v0.16b - v2.16b}, \[x0\]
**	ret
*/
mfloat8x16x3_t test_ld3q(const mfloat8_t *ptr)
{
  return vld3q_mf8(ptr);
}

/*
** test_ld3_dup:	{ target lp64 }
**	ld3r	{v0.8b - v2.8b}, \[x0\]
**	ret
*/
mfloat8x8x3_t test_ld3_dup(const mfloat8_t *ptr)
{
  return vld3_dup_mf8(ptr);
}

/*
** test_ld3q_dup:	{ target lp64 }
**	ld3r	{v0.16b - v2.16b}, \[x0\]
**	ret
*/
mfloat8x16x3_t test_ld3q_dup(const mfloat8_t *ptr)
{
  return vld3q_dup_mf8(ptr);
}

/*
** test_ld3_lane1:	{ target lp64 }
**	ld3	{v0.b - v2.b}\[0\], \[x0\]
**	ret
*/
mfloat8x8x3_t test_ld3_lane1(const mfloat8_t *ptr, mfloat8x8x3_t a)
{
  return vld3_lane_mf8(ptr, a, 0);
}

/*
** test_ld3_lane2:	{ target lp64 }
**	ld3	{v0.b - v2.b}\[7\], \[x0\]
**	ret
*/
mfloat8x8x3_t test_ld3_lane2(const mfloat8_t *ptr, mfloat8x8x3_t a)
{
  return vld3_lane_mf8(ptr, a, 7);
}

/*
** test_ld3q_lane1:	{ target lp64 }
**	ld3	{v0.b - v2.b}\[0\], \[x0\]
**	ret
*/
mfloat8x16x3_t test_ld3q_lane1(const mfloat8_t *ptr, mfloat8x16x3_t a)
{
  return vld3q_lane_mf8(ptr, a, 0);
}

/*
** test_ld3q_lane2:	{ target lp64 }
**	ld3	{v0.b - v2.b}\[15\], \[x0\]
**	ret
*/
mfloat8x16x3_t test_ld3q_lane2(const mfloat8_t *ptr, mfloat8x16x3_t a)
{
  return vld3q_lane_mf8(ptr, a, 15);
}

/*
** test_ld4:	{ target lp64 }
**	ld4	{v0.8b - v3.8b}, \[x0\]
**	ret
*/
mfloat8x8x4_t test_ld4(const mfloat8_t *ptr)
{
  return vld4_mf8(ptr);
}

/*
** test_ld4q:	{ target lp64 }
**	ld4	{v0.16b - v3.16b}, \[x0\]
**	ret
*/
mfloat8x16x4_t test_ld4q(const mfloat8_t *ptr)
{
  return vld4q_mf8(ptr);
}

/*
** test_ld4_dup:	{ target lp64 }
**	ld4r	{v0.8b - v3.8b}, \[x0\]
**	ret
*/
mfloat8x8x4_t test_ld4_dup(const mfloat8_t *ptr)
{
  return vld4_dup_mf8(ptr);
}

/*
** test_ld4q_dup:	{ target lp64 }
**	ld4r	{v0.16b - v3.16b}, \[x0\]
**	ret
*/
mfloat8x16x4_t test_ld4q_dup(const mfloat8_t *ptr)
{
  return vld4q_dup_mf8(ptr);
}

/*
** test_ld4_lane1:	{ target lp64 }
**	ld4	{v0.b - v3.b}\[0\], \[x0\]
**	ret
*/
mfloat8x8x4_t test_ld4_lane1(const mfloat8_t *ptr, mfloat8x8x4_t a)
{
  return vld4_lane_mf8(ptr, a, 0);
}

/*
** test_ld4_lane2:	{ target lp64 }
**	ld4	{v0.b - v3.b}\[7\], \[x0\]
**	ret
*/
mfloat8x8x4_t test_ld4_lane2(const mfloat8_t *ptr, mfloat8x8x4_t a)
{
  return vld4_lane_mf8(ptr, a, 7);
}

/*
** test_ld4q_lane1:	{ target lp64 }
**	ld4	{v0.b - v3.b}\[0\], \[x0\]
**	ret
*/
mfloat8x16x4_t test_ld4q_lane1(const mfloat8_t *ptr, mfloat8x16x4_t a)
{
  return vld4q_lane_mf8(ptr, a, 0);
}

/*
** test_ld4q_lane2:	{ target lp64 }
**	ld4	{v0.b - v3.b}\[15\], \[x0\]
**	ret
*/
mfloat8x16x4_t test_ld4q_lane2(const mfloat8_t *ptr, mfloat8x16x4_t a)
{
  return vld4q_lane_mf8(ptr, a, 15);
}

/*
** test_mov1:
**	dup	v0.8b, v1.b\[0\]
**	ret
*/
mfloat8x8_t test_mov1(mfloat8_t a, mfloat8_t b)
{
  return vmov_n_mf8(b);
}

/*
** test_mov2:
**	movi	v0.2s, #?0
**	ret
*/
mfloat8x8_t test_mov2()
{
  return vmov_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y);
}

/*
** test_mov3:
**	movi	v0.8b, #?0xf
**	ret
*/
mfloat8x8_t test_mov3()
{
  return vmov_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0x0f }).y);
}

/*
** test_movq1:
**	dup	v0.16b, v1.b\[0\]
**	ret
*/
mfloat8x16_t test_movq1(mfloat8_t a, mfloat8_t b)
{
  return vmovq_n_mf8(b);
}

/*
** test_movq2:
**	movi	v0.4s, #?0
**	ret
*/
mfloat8x16_t test_movq2()
{
  return vmovq_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y);
}

/*
** test_movq3:
**	movi	v0.16b, #?0xf
**	ret
*/
mfloat8x16_t test_movq3()
{
  return vmovq_n_mf8(((union { uint8_t x; mfloat8_t y; }) { 0x0f }).y);
}

/*
** test_rev16:
**	rev16	v0.8b, v1.8b
**	ret
*/
mfloat8x8_t test_rev16(mfloat8_t a, mfloat8x8_t b)
{
  return vrev16_mf8(b);
}

/*
** test_rev16q:
**	rev16	v0.16b, v1.16b
**	ret
*/
mfloat8x16_t test_rev16q(mfloat8_t a, mfloat8x16_t b)
{
  return vrev16q_mf8(b);
}

/*
** test_rev32:
**	rev32	v0.8b, v1.8b
**	ret
*/
mfloat8x8_t test_rev32(mfloat8_t a, mfloat8x8_t b)
{
  return vrev32_mf8(b);
}

/*
** test_rev32q:
**	rev32	v0.16b, v1.16b
**	ret
*/
mfloat8x16_t test_rev32q(mfloat8_t a, mfloat8x16_t b)
{
  return vrev32q_mf8(b);
}

/*
** test_rev64:
**	rev64	v0.8b, v1.8b
**	ret
*/
mfloat8x8_t test_rev64(mfloat8_t a, mfloat8x8_t b)
{
  return vrev64_mf8(b);
}

/*
** test_rev64q:
**	rev64	v0.16b, v1.16b
**	ret
*/
mfloat8x16_t test_rev64q(mfloat8_t a, mfloat8x16_t b)
{
  return vrev64q_mf8(b);
}

/*
** test_set_lane1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_set_lane1(mfloat8x8_t a, mfloat8_t b)
{
  return vset_lane_mf8(b, a, 0);
}

/*
** test_set_lane2:
**	ins	v0.b\[7\], v1.b\[0\]
**	ret
*/
mfloat8x8_t test_set_lane2(mfloat8x8_t a, mfloat8_t b)
{
  return vset_lane_mf8(b, a, 7);
}

/*
** test_set_lane3:	{ target lp64 }
**	ld1	{v0.b}\[3\], \[x0\]
**	ret
*/
mfloat8x8_t test_set_lane3(mfloat8x8_t a, const mfloat8_t *ptr)
{
  return vset_lane_mf8(*ptr, a, 3);
}

/*
** test_set_lane4:
**	ins	v0.b\[6\], wzr
**	ret
*/
mfloat8x8_t test_set_lane4(mfloat8x8_t a)
{
  return vset_lane_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y, a, 6);
}

/*
** test_setq_lane1:
**	ins	v0.b\[0\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_setq_lane1(mfloat8x16_t a, mfloat8_t b)
{
  return vsetq_lane_mf8(b, a, 0);
}

/*
** test_setq_lane2:
**	ins	v0.b\[15\], v1.b\[0\]
**	ret
*/
mfloat8x16_t test_setq_lane2(mfloat8x16_t a, mfloat8_t b)
{
  return vsetq_lane_mf8(b, a, 15);
}

/*
** test_setq_lane3:	{ target lp64 }
**	ld1	{v0.b}\[9\], \[x0\]
**	ret
*/
mfloat8x16_t test_setq_lane3(mfloat8x16_t a, const mfloat8_t *ptr)
{
  return vsetq_lane_mf8(*ptr, a, 9);
}

/*
** test_setq_lane4:
**	ins	v0.b\[14\], wzr
**	ret
*/
mfloat8x16_t test_setq_lane4(mfloat8x16_t a)
{
  return vsetq_lane_mf8(((union { uint8_t x; mfloat8_t y; }) { 0 }).y, a, 14);
}

/*
** test_st1:	{ target { le && lp64 } }
**	str	d0, \[x0\]
**	ret
*/
/*
** test_st1:	{ target { be && lp64 } }
**	st1	{v0.8b}, \[x0\]
**	ret
*/
void test_st1(mfloat8_t *ptr, mfloat8x8_t a)
{
  vst1_mf8(ptr, a);
}

/*
** test_st1q:	{ target { le && lp64 } }
**	str	q0, \[x0\]
**	ret
*/
/*
** test_st1q:	{ target { be && lp64 } }
**	st1	{v0.16b}, \[x0\]
**	ret
*/
void test_st1q(mfloat8_t *ptr, mfloat8x16_t a)
{
  vst1q_mf8(ptr, a);
}

/*
** test_st1_lane1:	{ target lp64 }
**	str	b0, \[x0\]
**	ret
*/
void test_st1_lane1(mfloat8_t *ptr, mfloat8x8_t a)
{
  vst1_lane_mf8(ptr, a, 0);
}

/*
** test_st1_lane2:	{ target lp64 }
**	st1	{v0.b}\[7\], \[x0\]
**	ret
*/
void test_st1_lane2(mfloat8_t *ptr, mfloat8x8_t a)
{
  vst1_lane_mf8(ptr, a, 7);
}

/*
** test_st1q_lane1:	{ target lp64 }
**	str	b0, \[x0\]
**	ret
*/
void test_st1q_lane1(mfloat8_t *ptr, mfloat8x16_t a)
{
  vst1q_lane_mf8(ptr, a, 0);
}

/*
** test_st1q_lane2:	{ target lp64 }
**	st1	{v0.b}\[15\], \[x0\]
**	ret
*/
void test_st1q_lane2(mfloat8_t *ptr, mfloat8x16_t a)
{
  vst1q_lane_mf8(ptr, a, 15);
}

/*
** test_st1_x2:	{ target lp64 }
**	st1	{v0.8b( - |, )v1.8b}, \[x0\]
**	ret
*/
void test_st1_x2(mfloat8_t *ptr, mfloat8x8x2_t a)
{
  vst1_mf8_x2(ptr, a);
}

/*
** test_st1q_x2:	{ target lp64 }
**	st1	{v0.16b( - |, )v1.16b}, \[x0\]
**	ret
*/
void test_st1q_x2(mfloat8_t *ptr, mfloat8x16x2_t a)
{
  vst1q_mf8_x2(ptr, a);
}

/*
** test_st1_x3:	{ target lp64 }
**	st1	{v0.8b - v2.8b}, \[x0\]
**	ret
*/
void test_st1_x3(mfloat8_t *ptr, mfloat8x8x3_t a)
{
  vst1_mf8_x3(ptr, a);
}

/*
** test_st1q_x3:	{ target lp64 }
**	st1	{v0.16b - v2.16b}, \[x0\]
**	ret
*/
void test_st1q_x3(mfloat8_t *ptr, mfloat8x16x3_t a)
{
  vst1q_mf8_x3(ptr, a);
}

/*
** test_st1_x4:	{ target lp64 }
**	st1	{v0.8b - v3.8b}, \[x0\]
**	ret
*/
void test_st1_x4(mfloat8_t *ptr, mfloat8x8x4_t a)
{
  vst1_mf8_x4(ptr, a);
}

/*
** test_st1q_x4:	{ target lp64 }
**	st1	{v0.16b - v3.16b}, \[x0\]
**	ret
*/
void test_st1q_x4(mfloat8_t *ptr, mfloat8x16x4_t a)
{
  vst1q_mf8_x4(ptr, a);
}

/*
** test_st2:	{ target lp64 }
**	st2	{v0.8b( - |, )v1.8b}, \[x0\]
**	ret
*/
void test_st2(mfloat8_t *ptr, mfloat8x8x2_t a)
{
  vst2_mf8(ptr, a);
}

/*
** test_st2q:	{ target lp64 }
**	st2	{v0.16b( - |, )v1.16b}, \[x0\]
**	ret
*/
void test_st2q(mfloat8_t *ptr, mfloat8x16x2_t a)
{
  vst2q_mf8(ptr, a);
}

/*
** test_st2_lane1:	{ target lp64 }
**	st2	{v0.b( - |, )v1.b}\[0\], \[x0\]
**	ret
*/
void test_st2_lane1(mfloat8_t *ptr, mfloat8x8x2_t a)
{
  vst2_lane_mf8(ptr, a, 0);
}

/*
** test_st2_lane2:	{ target lp64 }
**	st2	{v0.b( - |, )v1.b}\[7\], \[x0\]
**	ret
*/
void test_st2_lane2(mfloat8_t *ptr, mfloat8x8x2_t a)
{
  vst2_lane_mf8(ptr, a, 7);
}

/*
** test_st2q_lane1:	{ target lp64 }
**	st2	{v0.b( - |, )v1.b}\[0\], \[x0\]
**	ret
*/
void test_st2q_lane1(mfloat8_t *ptr, mfloat8x16x2_t a)
{
  vst2q_lane_mf8(ptr, a, 0);
}

/*
** test_st2q_lane2:	{ target lp64 }
**	st2	{v0.b( - |, )v1.b}\[15\], \[x0\]
**	ret
*/
void test_st2q_lane2(mfloat8_t *ptr, mfloat8x16x2_t a)
{
  vst2q_lane_mf8(ptr, a, 15);
}

/*
** test_st3:	{ target lp64 }
**	st3	{v0.8b - v2.8b}, \[x0\]
**	ret
*/
void test_st3(mfloat8_t *ptr, mfloat8x8x3_t a)
{
  vst3_mf8(ptr, a);
}

/*
** test_st3q:	{ target lp64 }
**	st3	{v0.16b - v2.16b}, \[x0\]
**	ret
*/
void test_st3q(mfloat8_t *ptr, mfloat8x16x3_t a)
{
  vst3q_mf8(ptr, a);
}

/*
** test_st3_lane1:	{ target lp64 }
**	st3	{v0.b - v2.b}\[0\], \[x0\]
**	ret
*/
void test_st3_lane1(mfloat8_t *ptr, mfloat8x8x3_t a)
{
  vst3_lane_mf8(ptr, a, 0);
}

/*
** test_st3_lane2:	{ target lp64 }
**	st3	{v0.b - v2.b}\[7\], \[x0\]
**	ret
*/
void test_st3_lane2(mfloat8_t *ptr, mfloat8x8x3_t a)
{
  vst3_lane_mf8(ptr, a, 7);
}

/*
** test_st3q_lane1:	{ target lp64 }
**	st3	{v0.b - v2.b}\[0\], \[x0\]
**	ret
*/
void test_st3q_lane1(mfloat8_t *ptr, mfloat8x16x3_t a)
{
  vst3q_lane_mf8(ptr, a, 0);
}

/*
** test_st3q_lane2:	{ target lp64 }
**	st3	{v0.b - v2.b}\[15\], \[x0\]
**	ret
*/
void test_st3q_lane2(mfloat8_t *ptr, mfloat8x16x3_t a)
{
  vst3q_lane_mf8(ptr, a, 15);
}

/*
** test_st4:	{ target lp64 }
**	st4	{v0.8b - v3.8b}, \[x0\]
**	ret
*/
void test_st4(mfloat8_t *ptr, mfloat8x8x4_t a)
{
  vst4_mf8(ptr, a);
}

/*
** test_st4q:	{ target lp64 }
**	st4	{v0.16b - v3.16b}, \[x0\]
**	ret
*/
void test_st4q(mfloat8_t *ptr, mfloat8x16x4_t a)
{
  vst4q_mf8(ptr, a);
}

/*
** test_st4_lane1:	{ target lp64 }
**	st4	{v0.b - v3.b}\[0\], \[x0\]
**	ret
*/
void test_st4_lane1(mfloat8_t *ptr, mfloat8x8x4_t a)
{
  vst4_lane_mf8(ptr, a, 0);
}

/*
** test_st4_lane2:	{ target lp64 }
**	st4	{v0.b - v3.b}\[7\], \[x0\]
**	ret
*/
void test_st4_lane2(mfloat8_t *ptr, mfloat8x8x4_t a)
{
  vst4_lane_mf8(ptr, a, 7);
}

/*
** test_st4q_lane1:	{ target lp64 }
**	st4	{v0.b - v3.b}\[0\], \[x0\]
**	ret
*/
void test_st4q_lane1(mfloat8_t *ptr, mfloat8x16x4_t a)
{
  vst4q_lane_mf8(ptr, a, 0);
}

/*
** test_st4q_lane2:	{ target lp64 }
**	st4	{v0.b - v3.b}\[15\], \[x0\]
**	ret
*/
void test_st4q_lane2(mfloat8_t *ptr, mfloat8x16x4_t a)
{
  vst4q_lane_mf8(ptr, a, 15);
}

/*
** test_tbl1:
**	fmov	d([0-9]+), d0
**	tbl	v0.8b, {v\1.16b}, v1.8b
**	ret
*/
mfloat8x8_t test_tbl1(mfloat8x8_t a, uint8x8_t b)
{
  return vtbl1_mf8(a, b);
}

/*
** test_tbl2:
**	uzp1	v([0-9]+).2d, v0.2d, v1.2d
**	tbl	v0.8b, {v\1.16b}, v2.8b
**	ret
*/
mfloat8x8_t test_tbl2(mfloat8x8x2_t a, uint8x8_t b)
{
  return vtbl2_mf8(a, b);
}

/*
** test_tbl3:
**	uzp1	v([0-9]+).2d, v0.2d, v1.2d
**	fmov	d([0-9]+), d2
**	tbl	v0.8b, {v\1.16b( - |, )v\2.16b}, v3.8b
**	ret
*/
mfloat8x8_t test_tbl3(mfloat8x8x3_t a, uint8x8_t b)
{
  return vtbl3_mf8(a, b);
}

/*
** test_tbl4:
**	uzp1	v([0-9]+).2d, v0.2d, v1.2d
**	uzp1	v([0-9]+).2d, v2.2d, v3.2d
**	tbl	v0.8b, {v\1.16b( - |, )v\2.16b}, v4.8b
**	ret
*/
mfloat8x8_t test_tbl4(mfloat8x8x4_t a, uint8x8_t b)
{
  return vtbl4_mf8(a, b);
}

/*
** test_qtbl1:
**	tbl	v0.8b, {v0.16b}, v1.8b
**	ret
*/
mfloat8x8_t test_qtbl1(mfloat8x16_t a, uint8x8_t b)
{
  return vqtbl1_mf8(a, b);
}

/*
** test_qtbl1q:
**	tbl	v0.16b, {v0.16b}, v1.16b
**	ret
*/
mfloat8x16_t test_qtbl1q(mfloat8x16_t a, uint8x16_t b)
{
  return vqtbl1q_mf8(a, b);
}

/*
** test_qtbl2:
**	tbl	v0.8b, {v0.16b( - |, )v1.16b}, v2.8b
**	ret
*/
mfloat8x8_t test_qtbl2(mfloat8x16x2_t a, uint8x8_t b)
{
  return vqtbl2_mf8(a, b);
}

/*
** test_qtbl2q:
**	tbl	v0.16b, {v0.16b( - |, )v1.16b}, v2.16b
**	ret
*/
mfloat8x16_t test_qtbl2q(mfloat8x16x2_t a, uint8x16_t b)
{
  return vqtbl2q_mf8(a, b);
}

/*
** test_qtbl3:
**	tbl	v0.8b, {v0.16b - v2.16b}, v3.8b
**	ret
*/
mfloat8x8_t test_qtbl3(mfloat8x16x3_t a, uint8x8_t b)
{
  return vqtbl3_mf8(a, b);
}

/*
** test_qtbl3q:
**	tbl	v0.16b, {v0.16b - v2.16b}, v3.16b
**	ret
*/
mfloat8x16_t test_qtbl3q(mfloat8x16x3_t a, uint8x16_t b)
{
  return vqtbl3q_mf8(a, b);
}

/*
** test_qtbl4:
**	tbl	v0.8b, {v0.16b - v3.16b}, v4.8b
**	ret
*/
mfloat8x8_t test_qtbl4(mfloat8x16x4_t a, uint8x8_t b)
{
  return vqtbl4_mf8(a, b);
}

/*
** test_qtbl4q:
**	tbl	v0.16b, {v0.16b - v3.16b}, v4.16b
**	ret
*/
mfloat8x16_t test_qtbl4q(mfloat8x16x4_t a, uint8x16_t b)
{
  return vqtbl4q_mf8(a, b);
}

/*
** test_tbx1:
**	fmov	d([0-9]+), d1
**	tbl	v[0-9]+.8b, {v\1.16b}, v2.8b
**	...
**	cmh[is]	[^\n]+
**	(bit|bif|bsl)	[^\n]+
**	ret
*/
mfloat8x8_t test_tbx1(mfloat8x8_t a, mfloat8x8_t b, uint8x8_t c)
{
  return vtbx1_mf8(a, b, c);
}

/*
** test_tbx2:
**	uzp1	v([0-9]+).2d, v1.2d, v2.2d
**	tbx	v[0-9]+.8b, {v\1.16b}, v3.8b
**	ret
*/
mfloat8x8_t test_tbx2(mfloat8x8_t a, mfloat8x8x2_t b, uint8x8_t c)
{
  return vtbx2_mf8(a, b, c);
}

/*
** test_tbx3:
**	uzp1	v([0-9]+).2d, v1.2d, v2.2d
**	fmov	d([0-9]+), d3
**	tbl	v[0-9]+.8b, {v\1.16b( - |, )v\2.16b}, v4.8b
**	...
**	cmh[is]	[^\n]+
**	(bit|bif|bsl)	[^\n]+
**	ret
*/
mfloat8x8_t test_tbx3(mfloat8x8_t a, mfloat8x8x3_t b, uint8x8_t c)
{
  return vtbx3_mf8(a, b, c);
}

/*
** test_tbx4:
**	uzp1	v([0-9]+).2d, v1.2d, v2.2d
**	uzp1	v([0-9]+).2d, v3.2d, v4.2d
**	tbx	v0.8b, {v\1.16b( - |, )v\2.16b}, v5.8b
**	ret
*/
mfloat8x8_t test_tbx4(mfloat8x8_t a, mfloat8x8x4_t b, uint8x8_t c)
{
  return vtbx4_mf8(a, b, c);
}

/*
** test_qtbx1:
**	tbx	v0.8b, {v1.16b}, v2.8b
**	ret
*/
mfloat8x8_t test_qtbx1(mfloat8x8_t a, mfloat8x16_t b, uint8x8_t c)
{
  return vqtbx1_mf8(a, b, c);
}

/*
** test_qtbx1q:
**	tbx	v0.16b, {v1.16b}, v2.16b
**	ret
*/
mfloat8x16_t test_qtbx1q(mfloat8x16_t a, mfloat8x16_t b, uint8x16_t c)
{
  return vqtbx1q_mf8(a, b, c);
}

/*
** test_qtbx2:
**	tbx	v0.8b, {v1.16b( - |, )v2.16b}, v3.8b
**	ret
*/
mfloat8x8_t test_qtbx2(mfloat8x8_t a, mfloat8x16x2_t b, uint8x8_t c)
{
  return vqtbx2_mf8(a, b, c);
}

/*
** test_qtbx2q:
**	tbx	v0.16b, {v1.16b( - |, )v2.16b}, v3.16b
**	ret
*/
mfloat8x16_t test_qtbx2q(mfloat8x16_t a, mfloat8x16x2_t b, uint8x16_t c)
{
  return vqtbx2q_mf8(a, b, c);
}

/*
** test_qtbx3:
**	tbx	v0.8b, {v1.16b - v3.16b}, v4.8b
**	ret
*/
mfloat8x8_t test_qtbx3(mfloat8x8_t a, mfloat8x16x3_t b, uint8x8_t c)
{
  return vqtbx3_mf8(a, b, c);
}

/*
** test_qtbx3q:
**	tbx	v0.16b, {v1.16b - v3.16b}, v4.16b
**	ret
*/
mfloat8x16_t test_qtbx3q(mfloat8x16_t a, mfloat8x16x3_t b, uint8x16_t c)
{
  return vqtbx3q_mf8(a, b, c);
}

/*
** test_qtbx4:
**	tbx	v0.8b, {v1.16b - v4.16b}, v5.8b
**	ret
*/
mfloat8x8_t test_qtbx4(mfloat8x8_t a, mfloat8x16x4_t b, uint8x8_t c)
{
  return vqtbx4_mf8(a, b, c);
}

/*
** test_qtbx4q:
**	tbx	v0.16b, {v1.16b - v4.16b}, v5.16b
**	ret
*/
mfloat8x16_t test_qtbx4q(mfloat8x16_t a, mfloat8x16x4_t b, uint8x16_t c)
{
  return vqtbx4q_mf8(a, b, c);
}

/*
** test_trn:
**	trn1	v0.8b, v2.8b, v3.8b
**	trn2	v1.8b, v2.8b, v3.8b
**	ret
*/
mfloat8x8x2_t test_trn(mfloat8_t a, mfloat8_t b, mfloat8x8_t c, mfloat8x8_t d)
{
  return vtrn_mf8(c, d);
}

/*
** test_trnq:
**	trn1	v0.16b, v2.16b, v3.16b
**	trn2	v1.16b, v2.16b, v3.16b
**	ret
*/
mfloat8x16x2_t test_trnq(mfloat8_t a, mfloat8_t b,
			 mfloat8x16_t c, mfloat8x16_t d)
{
  return vtrnq_mf8(c, d);
}

/*
** test_trn1:
**	trn1	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_trn1(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vtrn1_mf8(b, c);
}

/*
** test_trn1q:
**	trn1	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_trn1q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vtrn1q_mf8(b, c);
}

/*
** test_trn2:
**	trn2	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_trn2(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vtrn2_mf8(b, c);
}

/*
** test_trn2q:
**	trn2	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_trn2q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vtrn2q_mf8(b, c);
}

/*
** test_uzp:
**	uzp1	v0.8b, v2.8b, v3.8b
**	uzp2	v1.8b, v2.8b, v3.8b
**	ret
*/
mfloat8x8x2_t test_uzp(mfloat8_t a, mfloat8_t b, mfloat8x8_t c, mfloat8x8_t d)
{
  return vuzp_mf8(c, d);
}

/*
** test_uzpq:
**	uzp1	v0.16b, v2.16b, v3.16b
**	uzp2	v1.16b, v2.16b, v3.16b
**	ret
*/
mfloat8x16x2_t test_uzpq(mfloat8_t a, mfloat8_t b,
			 mfloat8x16_t c, mfloat8x16_t d)
{
  return vuzpq_mf8(c, d);
}

/*
** test_uzp1:
**	uzp1	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_uzp1(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vuzp1_mf8(b, c);
}

/*
** test_uzp1q:
**	uzp1	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_uzp1q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vuzp1q_mf8(b, c);
}

/*
** test_uzp2:
**	uzp2	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_uzp2(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vuzp2_mf8(b, c);
}

/*
** test_uzp2q:
**	uzp2	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_uzp2q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vuzp2q_mf8(b, c);
}

/*
** test_zip:
**	zip1	v0.8b, v2.8b, v3.8b
**	zip2	v1.8b, v2.8b, v3.8b
**	ret
*/
mfloat8x8x2_t test_zip(mfloat8_t a, mfloat8_t b, mfloat8x8_t c, mfloat8x8_t d)
{
  return vzip_mf8(c, d);
}

/*
** test_zipq:
**	zip1	v0.16b, v2.16b, v3.16b
**	zip2	v1.16b, v2.16b, v3.16b
**	ret
*/
mfloat8x16x2_t test_zipq(mfloat8_t a, mfloat8_t b,
			 mfloat8x16_t c, mfloat8x16_t d)
{
  return vzipq_mf8(c, d);
}

/*
** test_zip1:
**	zip1	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_zip1(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vzip1_mf8(b, c);
}

/*
** test_zip1q:
**	zip1	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_zip1q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vzip1q_mf8(b, c);
}

/*
** test_zip2:
**	zip2	v0.8b, v1.8b, v2.8b
**	ret
*/
mfloat8x8_t test_zip2(mfloat8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vzip2_mf8(b, c);
}

/*
** test_zip2q:
**	zip2	v0.16b, v1.16b, v2.16b
**	ret
*/
mfloat8x16_t test_zip2q(mfloat8_t a, mfloat8x16_t b, mfloat8x16_t c)
{
  return vzip2q_mf8(b, c);
}
