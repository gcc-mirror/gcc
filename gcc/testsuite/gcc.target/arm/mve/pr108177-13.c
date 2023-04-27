/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test:
**...
**	vstrht.16	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
**	vstrht.16	q[0-9]+, \[(?:ip|fp|r[0-9]+)\]
**...
*/

#define TYPE float16x8_t
#define INTRINSIC vstrhq_f16
#define INTRINSIC_P vstrhq_p_f16

#include "pr108177.x"
