/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O -msve-vector-bits=512 -fdump-tree-optimized" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** load_vl1:
**	ptrue	(p[0-7])\.[bhsd], vl1
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl1 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 1), ptr);
}

/*
** load_vl2:
**	ptrue	(p[0-7])\.h, vl2
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl2 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 2), ptr);
}

/*
** load_vl3:
**	ptrue	(p[0-7])\.h, vl3
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl3 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 3), ptr);
}

/*
** load_vl4:
**	ptrue	(p[0-7])\.h, vl4
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl4 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 4), ptr);
}

/*
** load_vl5:
**	ptrue	(p[0-7])\.h, vl5
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl5 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 5), ptr);
}

/*
** load_vl6:
**	ptrue	(p[0-7])\.h, vl6
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl6 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 6), ptr);
}

/*
** load_vl7:
**	ptrue	(p[0-7])\.h, vl7
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl7 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 7), ptr);
}

/*
** load_vl8:
**	ptrue	(p[0-7])\.h, vl8
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl8 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 8), ptr);
}

/*
** load_vl9:
**	mov	(x[0-9]+), #?9
**	whilelo	(p[0-7])\.h, xzr, \1
**	ld1h	z0\.h, \2/z, \[x0\]
**	ret
*/
svint16_t
load_vl9 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 9), ptr);
}

/*
** load_vl15:
**	mov	(x[0-9]+), #?15
**	whilelo	(p[0-7])\.h, xzr, \1
**	ld1h	z0\.h, \2/z, \[x0\]
**	ret
*/
svint16_t
load_vl15 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 15), ptr);
}

/*
** load_vl16:
**	ptrue	(p[0-7])\.h, vl16
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
svint16_t
load_vl16 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 16), ptr);
}

/*
** load_vl17:
**	mov	(x[0-9]+), #?17
**	whilelo	(p[0-7])\.h, xzr, \1
**	ld1h	z0\.h, \2/z, \[x0\]
**	ret
*/
svint16_t
load_vl17 (int16_t *ptr)
{
  return svld1 (svwhilelt_b16 (0, 17), ptr);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } } */
