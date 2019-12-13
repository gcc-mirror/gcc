/* { dg-additional-options "-O" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** cmp1:
**	ptrue	(p[0-7])\.b(?:[^\n]*)
**	cmple	p0\.b, \1/z, z0\.b, z1\.d
**	ret
*/
svbool_t
cmp1 (svint8_t x, svint64_t y)
{
  svbool_t res = svcmple_wide (svptrue_b8 (), x, y);
  svuint8_t res_u8 = svdup_u8_z (res, 1);
  return svcmpne (svptrue_b8 (), res_u8, 0);
}

/*
** cmp2:
**	ptrue	(p[0-7])\.b(?:[^\n]*)
**	cmplt	p0\.b, \1/z, z0\.b, z1\.d
**	ret
*/
svbool_t
cmp2 (svint8_t x, svint64_t y)
{
  svbool_t res = svcmplt_wide (svptrue_b8 (), x, y);
  svuint8_t res_u8 = svdup_u8_z (res, 42);
  return svcmpeq (svptrue_b8 (), res_u8, 42);
}

#ifdef __cplusplus
}
#endif
