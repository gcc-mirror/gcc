/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test:
**...
**	vstrwt.32	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
**	vstrwt.32	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
*/

#define TYPE float32x4_t
#define INTRINSIC vstrwq_f32
#define INTRINSIC_P vstrwq_p_f32

#include "pr108177.x"
