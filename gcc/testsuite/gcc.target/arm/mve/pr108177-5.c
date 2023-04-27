/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test:
**...
**	vstrbt.32	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
**	vstrbt.32	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
*/

#define TYPE uint32x4_t
#define INTRINSIC vstrbq_u32
#define INTRINSIC_P vstrbq_p_u32

#include "pr108177.x"
