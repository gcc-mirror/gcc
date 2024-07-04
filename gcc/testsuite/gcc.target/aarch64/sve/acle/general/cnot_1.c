/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** foo:
**	cmpeq	(p[0-7])\.s, p0/z, z0\.s, #0
**	mov	z0\.s, \1/z, #1
**	ret
*/
svint32_t foo(svbool_t pg, svint32_t y)
{
  return svsel(svcmpeq(pg, y, 0), svdup_s32(1), svdup_s32(0));
}

#ifdef __cplusplus
}
#endif
