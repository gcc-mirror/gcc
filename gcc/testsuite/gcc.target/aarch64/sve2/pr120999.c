/* PR target/120999.  */
/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

#define NOR(x, y)   (~((x) | (y)))

/*
** nor_z:
** 	movprfx	z0, z1
**	nbsl	z0.d, z0.d, z2.d, z1.d
** 	ret
*/
svuint64_t nor_z(svuint64_t c, svuint64_t a, svuint64_t b) { return NOR(a, b); }

