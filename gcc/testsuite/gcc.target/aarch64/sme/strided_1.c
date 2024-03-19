// { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sme.h>

#pragma GCC target "+sme2"

// This file deliberately contains nonsense code.

/*
** test1:
**	ptrue	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	ret
*/
void test1(int32_t *dest, int32_t *src) __arm_streaming
{
  svcount_t pg = svptrue_c32();
  svint32x4_t l0 = svld1_vnum_x4(pg, src, 0);
  svint32x4_t l1 = svld1_vnum_x4(pg, src, 4);
  svint32x4_t l2 = svld1_vnum_x4(pg, src, 8);
  svint32x4_t l3 = svld1_vnum_x4(pg, src, 12);
  svst1_vnum(pg, dest, 0,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 4,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 8,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 12,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
}

/*
** test2:
**	ptrue	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	ld1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	st1w	[^\n]+
**	ret
*/
void test2(int32_t *dest, int32_t *src) __arm_streaming
{
  svcount_t pg = svptrue_c32();
  svint32x4_t l0 = svld1_vnum_x4(pg, src, 0);
  svint32x4_t l1 = svld1_vnum_x4(pg, src, 4);
  svint32x4_t l2 = svld1_vnum_x4(pg, src, 8);
  svint32x4_t l3 = svld1_vnum_x4(pg, src, 12);
  svst1_vnum(pg, dest, 0,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 4,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 8,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 12,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
  svst1_vnum(pg, dest, 16,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 20,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 24,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 28,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
}

/*
** test3:
**	ptrue	([^\n]+)\.s
**	ld1w	{z16\.s - z19\.s}, \1/z, \[x1\]
**	ld1w	{z20\.s - z23\.s}, \1/z, \[x1, #4, mul vl\]
**	ld1w	{z24\.s - z27\.s}, \1/z, \[x1, #8, mul vl\]
**	ld1w	{z28\.s - z31\.s}, \1/z, \[x1, #12, mul vl\]
**	sclamp	[^\n]+
**	st1w	{z19\.s, z23\.s, z27\.s, z31\.s}, \1, \[x0, #12, mul vl\]
**	st1w	{z16\.s, z20\.s, z24\.s, z28\.s}, \1, \[x0\]
**	st1w	{z17\.s, z21\.s, z25\.s, z29\.s}, \1, \[x0, #4, mul vl\]
**	st1w	{z18\.s, z22\.s, z26\.s, z30\.s}, \1, \[x0, #8, mul vl\]
**	ret
*/
void test3(int32_t *dest, int32_t *src) __arm_streaming
{
  svcount_t pg = svptrue_c32();
  svint32x4_t al0 = svld1_vnum_x4(pg, src, 0);
  svint32x4_t l1 = svld1_vnum_x4(pg, src, 4);
  svint32x4_t l2 = svld1_vnum_x4(pg, src, 8);
  svint32x4_t l3 = svld1_vnum_x4(pg, src, 12);
  svint32x4_t l0 = svclamp(al0, svget4(l3, 0), svget4(l3, 1));
  svst1_vnum(pg, dest, 12,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
  svst1_vnum(pg, dest, 0,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 4,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 8,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
}

/*
** test4:
**	ptrue	([^\n]+)\.s
**	ld1w	{z16\.s - z19\.s}, \1/z, \[x1\]
**	ld1w	{z20\.s - z23\.s}, \1/z, \[x1, #4, mul vl\]
**	ld1w	{z24\.s - z27\.s}, \1/z, \[x1, #8, mul vl\]
**	ld1w	{z28\.s - z31\.s}, \1/z, \[x1, #12, mul vl\]
**	sclamp	[^\n]+
**	st1w	{z16\.s, z20\.s, z24\.s, z28\.s}, \1, \[x0\]
**	st1w	{z17\.s, z21\.s, z25\.s, z29\.s}, \1, \[x0, #4, mul vl\]
**	st1w	{z18\.s, z22\.s, z26\.s, z30\.s}, \1, \[x0, #8, mul vl\]
**	st1w	{z19\.s, z23\.s, z27\.s, z31\.s}, \1, \[x0, #12, mul vl\]
**	st1w	{z16\.s, z20\.s, z24\.s, z28\.s}, \1, \[x0, #16, mul vl\]
**	st1w	{z17\.s, z21\.s, z25\.s, z29\.s}, \1, \[x0, #20, mul vl\]
**	st1w	{z18\.s, z22\.s, z26\.s, z30\.s}, \1, \[x0, #24, mul vl\]
**	st1w	{z19\.s, z23\.s, z27\.s, z31\.s}, \1, \[x0, #28, mul vl\]
**	...
**	ret
*/
void test4(int32_t *dest, int32_t *src) __arm_streaming
{
  svcount_t pg = svptrue_c32();
  svint32x4_t l0 = svld1_vnum_x4(pg, src, 0);
  svint32x4_t l1 = svld1_vnum_x4(pg, src, 4);
  svint32x4_t l2 = svld1_vnum_x4(pg, src, 8);
  svint32x4_t l3 = svld1_vnum_x4(pg, src, 12);
  l0 = svclamp(l0, svget4(l3, 0), svget4(l3, 1));
  svst1_vnum(pg, dest, 0,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 4,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 8,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 12,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
  svst1_vnum(pg, dest, 16,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 20,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 24,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 28,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
}

/*
** test5:
**	ptrue	[^\n]+
**	ld1b	[^\n]+
**	ld1b	[^\n]+
**	ptrue	([^\n]+)\.s
**	ld1w	[^\n]+, \1/z, \[x0\]
**	luti4	{z16\.s, z20\.s, z24\.s, z28\.s}, zt0, z[0-9]+\[0\]
**	luti4	{z17\.s, z21\.s, z25\.s, z29\.s}, zt0, z[0-9]+\[1\]
**	luti4	{z18\.s, z22\.s, z26\.s, z30\.s}, zt0, z[0-9]+\[0\]
**	luti4	{z19\.s, z23\.s, z27\.s, z31\.s}, zt0, z[0-9]+\[1\]
**	uclamp	{z16\.s - z19\.s}, z[0-9]+\.s, z[0-9]+\.s
**	uclamp	{z20\.s - z23\.s}, z[0-9]+\.s, z[0-9]+\.s
**	uclamp	{z24\.s - z27\.s}, z[0-9]+\.s, z[0-9]+\.s
**	uclamp	{z28\.s - z31\.s}, z[0-9]+\.s, z[0-9]+\.s
**	st1w	{z16\.s - z19\.s}, \1, \[x0\]
**	st1w	{z20\.s - z23\.s}, \1, \[x0, #4, mul vl\]
**	st1w	{z24\.s - z27\.s}, \1, \[x0, #8, mul vl\]
**	st1w	{z28\.s - z31\.s}, \1, \[x0, #12, mul vl\]
**	ret
*/
void test5(uint32_t *dest, uint8_t *indices)
  __arm_streaming __arm_preserves("za") __arm_inout("zt0")
{
  svuint8_t indices1 = svld1_vnum(svptrue_b8(), indices, 0);
  svuint8_t indices2 = svld1_vnum(svptrue_b8(), indices, 2);

  svcount_t pg = svptrue_c32();
  svuint32x4_t bounds = svld1_x4(pg, dest);

  svuint32x4_t x0 = svluti4_lane_zt_u32_x4(0, indices1, 0);
  svuint32x4_t x1 = svluti4_lane_zt_u32_x4(0, indices1, 1);
  svuint32x4_t x2 = svluti4_lane_zt_u32_x4(0, indices2, 0);
  svuint32x4_t x3 = svluti4_lane_zt_u32_x4(0, indices2, 1);

  svuint32x4_t y0 = svcreate4(svget4(x0, 0), svget4(x1, 0),
			      svget4(x2, 0), svget4(x3, 0));
  svuint32x4_t y1 = svcreate4(svget4(x0, 1), svget4(x1, 1),
			      svget4(x2, 1), svget4(x3, 1));
  svuint32x4_t y2 = svcreate4(svget4(x0, 2), svget4(x1, 2),
			      svget4(x2, 2), svget4(x3, 2));
  svuint32x4_t y3 = svcreate4(svget4(x0, 3), svget4(x1, 3),
			      svget4(x2, 3), svget4(x3, 3));

  y0 = svclamp(y0, svget4(bounds, 0), svget4(bounds, 1));
  y1 = svclamp(y1, svget4(bounds, 2), svget4(bounds, 3));
  y2 = svclamp(y2, svget4(bounds, 0), svget4(bounds, 1));
  y3 = svclamp(y3, svget4(bounds, 2), svget4(bounds, 3));

  svst1_vnum(pg, dest, 0, y0);
  svst1_vnum(pg, dest, 4, y1);
  svst1_vnum(pg, dest, 8, y2);
  svst1_vnum(pg, dest, 12, y3);
}

/*
** test6:
**	ptrue	[^\n]+
**	ld1h	[^\n]+
**	sclamp	[^\n]+
**	st1h	[^\n]+
**	ret
*/
void test6(int16_t *ptr)
  __arm_streaming __arm_preserves("za") __arm_inout("zt0")
{
  svcount_t pg = svptrue_c16();
  svint16x4_t x0 = svld1_x4(pg, ptr);
  x0 = svclamp(x0, svget4(x0, 0), svget4(x0, 3));
  svst1(pg, ptr, x0);
}
