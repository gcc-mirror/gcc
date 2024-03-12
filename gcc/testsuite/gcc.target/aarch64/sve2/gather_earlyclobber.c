/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** foownt:
**	ldnt1w	(?:z[1-9][0-9]*)\.s, p0/z, \[z0\.s\]
**	...
**	ret
*/

svuint32_t
foownt (svbool_t p, svuint32_t bases, svuint32_t a)
{
  return svadd_u32_x (p, a, svldnt1_gather_u32 (p, bases));
}

/*
** foodbnt:
**	ldnt1d	(?:z[1-9][0-9]*)\.d, p0/z, \[z0\.d\]
**	...
**	ret
*/

svuint64_t
foodbnt (svbool_t p, svuint64_t bases, svuint64_t a)
{
  return svadd_u64_x (p, a, svldnt1_gather_u64 (p, bases));
}

