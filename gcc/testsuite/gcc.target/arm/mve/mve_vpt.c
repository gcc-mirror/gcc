/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-final { check-function-bodies "**" "" } } */
#include <arm_mve.h>
void test0 (uint8_t *a, uint8_t *b, uint8_t *c)
{
    uint8x16_t va = vldrbq_u8 (a);
    uint8x16_t vb = vldrbq_u8 (b);
    mve_pred16_t p = vcmpeqq_u8 (va, vb);
    uint8x16_t vc = vaddq_x_u8 (va, vb, p);
    vstrbq_p_u8 (c, vc, p);
}
/*
** test0:
**	vldrb.8	q[0-9]+, \[r[0-9]+\]
**	vldrb.8	q[0-9]+, \[r[0-9]+\]
**	vcmp.i8	eq, q[0-9]+, q[0-9]+
**	vpst
**	vaddt.i8	(q[0-9]+), q[0-9]+, q[0-9]+
**	vpst
**	vstrbt.8	\1, \[r[0-9]+\]
**	bx	lr
*/
