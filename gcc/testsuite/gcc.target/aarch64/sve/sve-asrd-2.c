/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-isel" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-tree-dump-times {\.DIV_POW2} 4 "isel" } } */

#include <arm_sve.h>

#define FOO(TYPE) \
TYPE foo_##TYPE (TYPE x) \
{ \
  return x / 8; \
}

/*
** foo_int32x4_t:
**	ptrue	(p[0-7]).b, vl16
**	asrd	z[0-9]+\.s, \1/m, z[0-9]+\.s, #3
**	ret
*/
FOO (int32x4_t)

/*
** foo_int64x2_t:
**	ptrue	(p[0-7]).b, vl16
**	asrd	z[0-9]+\.d, \1/m, z[0-9]+\.d, #3
**	ret
*/
FOO (int64x2_t)

/*
** foo_svint32_t:
**	ptrue	(p[0-7]).b, all
**	asrd	z[0-9]+\.s, \1/m, z[0-9]+\.s, #3
**	ret
*/
FOO (svint32_t)

/*
** foo_svint64_t:
**	ptrue	(p[0-7]).b, all
**	asrd	z[0-9]+\.d, \1/m, z[0-9]+\.d, #3
**	ret
*/
FOO (svint64_t)

/*
** foo_nonpow2:
**	mov	(z[0-9]+\.s), #7
**	ptrue	(p[0-7]).b, all
**	sdiv	(z[0-9]+\.s), \2/m, \3, \1
**	ret
*/
svint32_t
foo_nonpow2 (svint32_t x)
{
  return x / 7;
}

/*
** foo_unsigned:
**	mov	(z[0-9]+\.s), #8
**	ptrue	(p[0-7]).b, all
**	udiv	(z[0-9]+\.s), \2/m, \3, \1
**	ret
*/
svuint32_t
foo_unsigned (svuint32_t x)
{
  return x / 8;
}
