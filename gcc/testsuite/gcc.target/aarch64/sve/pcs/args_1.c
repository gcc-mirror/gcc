/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>

/*
** callee_pred:
**	addvl	sp, sp, #-1
**	str	p[0-9]+, \[sp\]
**	str	p[0-9]+, \[sp, #1, mul vl\]
**	ldr	(p[0-9]+), \[x0\]
**	ldr	(p[0-9]+), \[x1\]
**	brkpa	(p[0-7])\.b, p0/z, p1\.b, p2\.b
**	brkpb	(p[0-7])\.b, \3/z, p3\.b, \1\.b
**	brka	p0\.b, \4/z, \2\.b
**	ldr	p[0-9]+, \[sp\]
**	ldr	p[0-9]+, \[sp, #1, mul vl\]
**	addvl	sp, sp, #1
**	ret
*/
__SVBool_t __attribute__((noipa))
callee_pred (__SVBool_t p0, __SVBool_t p1, __SVBool_t p2, __SVBool_t p3,
	     __SVBool_t mem0, __SVBool_t mem1)
{
  p0 = svbrkpa_z (p0, p1, p2);
  p0 = svbrkpb_z (p0, p3, mem0);
  return svbrka_z (p0, mem1);
}

/*
** caller_pred:
**	...
**	ptrue	(p[0-9]+)\.b, vl5
**	str	\1, \[x0\]
**	...
**	ptrue	(p[0-9]+)\.h, vl6
**	str	\2, \[x1\]
**	ptrue	p3\.d, vl4
**	ptrue	p2\.s, vl3
**	ptrue	p1\.h, vl2
**	ptrue	p0\.b, vl1
**	bl	callee_pred
**	...
*/
__SVBool_t __attribute__((noipa))
caller_pred (void)
{
  return callee_pred (svptrue_pat_b8 (SV_VL1),
		      svptrue_pat_b16 (SV_VL2),
		      svptrue_pat_b32 (SV_VL3),
		      svptrue_pat_b64 (SV_VL4),
		      svptrue_pat_b8 (SV_VL5),
		      svptrue_pat_b16 (SV_VL6));
}
