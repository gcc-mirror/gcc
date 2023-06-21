/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** food:
**	ld1d	(?:z[1-9][0-9]*)\.d, p0/z, \[x0, z0\.d\]
**	...
**	ret
*/

svuint64_t
food (svbool_t p, uint64_t *in, svint64_t offsets, svuint64_t a)
{
  return svadd_u64_x (p, a, svld1_gather_offset(p, in, offsets));
}

/*
** foodb:
**	ld1d	(?:z[1-9][0-9]*)\.d, p0/z, \[z0\.d\]
**	...
**	ret
*/

svuint64_t
foodb (svbool_t p, svuint64_t bases, svuint64_t a)
{
  return svadd_u64_x (p, a, svld1_gather_u64 (p, bases));
}

/*
** foodff:
**	ldff1d	(?:z[1-9][0-9]*)\.d, p0/z, \[z0\.d\]
**	...
**	ret
*/

svuint64_t
foodff (svbool_t p, svuint64_t bases, svuint64_t a)
{
  return svadd_u64_x (p, a, svldff1_gather_u64 (p, bases));
}

/*
** foow:
**	ld1w	(?:z[1-9][0-9]*)\.s, p0/z, \[z0\.s\]
**	...
**	ret
*/

svuint32_t
foow (svbool_t p, svuint32_t bases, svuint32_t a)
{
  return svadd_u32_x (p, a, svld1_gather_u32 (p, bases));
}

/*
** foowff:
**	ldff1w	(?:z[1-9][0-9]*)\.s, p0/z, \[z0\.s\]
**	...
**	ret
*/

svuint32_t
foowff (svbool_t p, svuint32_t bases, svuint32_t a)
{
  return svadd_u32_x (p, a, svldff1_gather_u32 (p, bases));
}

/*
** fooubd:
**	ld1b	(?:z[1-9][0-9]*)\.d, p0/z, \[x0, z0\.d\]
**	...
**	ret
*/

svuint64_t
fooubd (svbool_t p, uint8_t *base, svuint64_t offsets, svuint64_t a)
{
  return svadd_u64_x (p, a, svld1ub_gather_offset_u64 (p, base, offsets));
}

/*
** foosbd:
**	ld1sb	(?:z[1-9][0-9]*)\.d, p0/z, \[x0, z0\.d\]
**	...
**	ret
*/
svint64_t
foosbd (svbool_t p, int8_t *base, svint64_t offsets, svint64_t a)
{
  return svadd_s64_x (p, a, svld1sb_gather_offset_s64 (p, base, offsets));
}

