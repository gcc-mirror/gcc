/* PR target/121599.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** foo:
**	movi?	[vdz]([0-9]+)\.?b?, #0
**	movprfx	z0\.b, p0/z, z0\.b
**	usqadd	z0\.b, p0/m, z0\.b, z\1\.b
**	ret
*/
svuint8_t foo (svbool_t pg, svuint8_t op1)
{
    return svsqadd_u8_z (pg, op1, svdup_s8 (0));
}

/*
** bar:
**	movi?	[vdz]([0-9]+)\.?b?, #0
**	movprfx	z0\.b, p0/z, z0\.b
**	suqadd	z0\.b, p0/m, z0\.b, z\1\.b
**	ret
*/
svint8_t bar (svbool_t pg, svint8_t op1)
{
    return svuqadd_n_s8_z (pg, op1, 0);
}

