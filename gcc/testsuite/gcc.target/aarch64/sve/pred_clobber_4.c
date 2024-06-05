/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC target "+sve"

#include <arm_sve.h>

extern void use(svbool_t);

/*
** foo:
**	...
**	ptrue	p0.b, all
**	cmplo	p0.h, p0/z, z0.h, z[0-9]+.h
**	...
*/
void foo (svuint16_t a, uint16_t b)
{
    svbool_t p0 = svcmplt_n_u16 (svptrue_b16 (), a, b);
    use (p0);
}
