// { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sme.h>

#pragma GCC target "+sme2"

// This file deliberately contains nonsense code.

/*
** test1:
**	ptrue	(pn[0-9]+)\.s
**	ld1w	{z16\.s - z19\.s}, \1/z, \[x1\]
**	ld1w	{z20\.s - z23\.s}, \1/z, \[x1, #4, mul vl\]
**	ld1w	{z24\.s - z27\.s}, \1/z, \[x1, #8, mul vl\]
**	ld1w	{z28\.s - z31\.s}, \1/z, \[x1, #12, mul vl\]
**	ptrue	[^\n]+
**	ld1rqw	[^\n]+
**	ld1rqw	[^\n]+
**	sclamp	{z16.s - z19.s}, [^\n]+
**	sclamp	{z20.s - z23.s}, [^\n]+
**	sclamp	{z24.s - z27.s}, [^\n]+
**	sclamp	{z28.s - z31.s}, [^\n]+
**	st1w	{z16\.s, z20\.s, z24\.s, z28\.s}, \1, \[x0\]
**	st1w	{z17\.s, z21\.s, z25\.s, z29\.s}, \1, \[x0, #4, mul vl\]
**	st1w	{z18\.s, z22\.s, z26\.s, z30\.s}, \1, \[x0, #8, mul vl\]
**	st1w	{z19\.s, z23\.s, z27\.s, z31\.s}, \1, \[x0, #12, mul vl\]
**	st1w	{z16\.s, z20\.s, z24\.s, z28\.s}, \1, \[x0, #16, mul vl\]
**	st1w	{z17\.s, z21\.s, z25\.s, z29\.s}, \1, \[x0, #20, mul vl\]
**	st1w	{z18\.s, z22\.s, z26\.s, z30\.s}, \1, \[x0, #24, mul vl\]
**	st1w	{z19\.s, z23\.s, z27\.s, z31\.s}, \1, \[x0, #28, mul vl\]
**	ld1w	{z16\.s - z19\.s}, \1/z, \[x3\]
**	ld1w	{z20\.s - z23\.s}, \1/z, \[x3, #4, mul vl\]
**	ld1w	{z24\.s - z27\.s}, \1/z, \[x3, #8, mul vl\]
**	ld1w	{z28\.s - z31\.s}, \1/z, \[x3, #12, mul vl\]
**	sclamp	{z16.s - z19.s}, [^\n]+
**	sclamp	{z20.s - z23.s}, [^\n]+
**	sclamp	{z24.s - z27.s}, [^\n]+
**	sclamp	{z28.s - z31.s}, [^\n]+
**	...
**	ret
*/
void test1(int32_t *dest, int32_t *src1, int32_t *src2,
	   int32_t *src3) __arm_streaming
{
  svcount_t pg = svptrue_c32();
  svint32x4_t l0 = svld1_vnum_x4(pg, src1, 0);
  svint32x4_t l1 = svld1_vnum_x4(pg, src1, 4);
  svint32x4_t l2 = svld1_vnum_x4(pg, src1, 8);
  svint32x4_t l3 = svld1_vnum_x4(pg, src1, 12);
  svint32_t l4 = svld1rq(svptrue_b32(), src2);
  svint32_t l5 = svld1rq(svptrue_b32(), src2 + 4);
  l0 = svclamp(l0, l4, l5);
  l1 = svclamp(l1, l4, l5);
  l2 = svclamp(l2, l4, l5);
  l3 = svclamp(l3, l4, l5);
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
  l0 = svld1_vnum_x4(pg, src3, 0);
  l1 = svld1_vnum_x4(pg, src3, 4);
  l2 = svld1_vnum_x4(pg, src3, 8);
  l3 = svld1_vnum_x4(pg, src3, 12);
  l0 = svclamp(l0, l4, l5);
  l1 = svclamp(l1, l4, l5);
  l2 = svclamp(l2, l4, l5);
  l3 = svclamp(l3, l4, l5);
  svst1_vnum(pg, dest, 32,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 36,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 40,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 44,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
  svst1_vnum(pg, dest, 48,
	     svcreate4(svget4(l0, 0), svget4(l1, 0),
		       svget4(l2, 0), svget4(l3, 0)));
  svst1_vnum(pg, dest, 52,
	     svcreate4(svget4(l0, 1), svget4(l1, 1),
		       svget4(l2, 1), svget4(l3, 1)));
  svst1_vnum(pg, dest, 56,
	     svcreate4(svget4(l0, 2), svget4(l1, 2),
		       svget4(l2, 2), svget4(l3, 2)));
  svst1_vnum(pg, dest, 60,
	     svcreate4(svget4(l0, 3), svget4(l1, 3),
		       svget4(l2, 3), svget4(l3, 3)));
}

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
