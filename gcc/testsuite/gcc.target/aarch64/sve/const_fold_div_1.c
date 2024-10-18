/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include "arm_sve.h"

/*
** s64_x_pg:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_x_pg (svbool_t pg)
{
  return svdiv_x (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_pg_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_0 (svbool_t pg)
{
  return svdiv_x (pg, svdup_s64 (0), svdup_s64 (3));
}

/*
** s64_x_pg_by0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_by0 (svbool_t pg)
{
  return svdiv_x (pg, svdup_s64 (5), svdup_s64 (0));
}

/*
** s64_z_pg:
**	mov	z[0-9]+\.d, p[0-7]/z, #1
**	ret
*/
svint64_t s64_z_pg (svbool_t pg)
{
  return svdiv_z (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_z_pg_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_0 (svbool_t pg)
{
  return svdiv_z (pg, svdup_s64 (0), svdup_s64 (3));
}

/*
** s64_z_pg_by0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_by0 (svbool_t pg)
{
  return svdiv_z (pg, svdup_s64 (5), svdup_s64 (0));
}

/*
** s64_m_pg:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	sdiv	\2, p[0-7]/m, \2, \1
**	ret
*/
svint64_t s64_m_pg (svbool_t pg)
{
  return svdiv_m (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_x_ptrue ()
{
  return svdiv_x (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_z_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_z_ptrue ()
{
  return svdiv_z (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_m_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_m_ptrue ()
{
  return svdiv_m (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_pg_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_x_pg_n (svbool_t pg)
{
  return svdiv_n_s64_x (pg, svdup_s64 (5), 3);
}

/*
** s64_x_pg_n_s64_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_n_s64_0 (svbool_t pg)
{
  return svdiv_n_s64_x (pg, svdup_s64 (0), 3);
}

/*
** s64_x_pg_n_s64_by0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_n_s64_by0 (svbool_t pg)
{
  return svdiv_n_s64_x (pg, svdup_s64 (5), 0);
}

/*
** s64_z_pg_n:
**	mov	z[0-9]+\.d, p[0-7]/z, #1
**	ret
*/
svint64_t s64_z_pg_n (svbool_t pg)
{
  return svdiv_n_s64_z (pg, svdup_s64 (5), 3);
}

/*
** s64_z_pg_n_s64_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_n_s64_0 (svbool_t pg)
{
  return svdiv_n_s64_z (pg, svdup_s64 (0), 3);
}

/*
** s64_z_pg_n_s64_by0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_n_s64_by0 (svbool_t pg)
{
  return svdiv_n_s64_z (pg, svdup_s64 (5), 0);
}

/*
** s64_m_pg_n:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	sdiv	\2, p[0-7]/m, \2, \1
**	ret
*/
svint64_t s64_m_pg_n (svbool_t pg)
{
  return svdiv_n_s64_m (pg, svdup_s64 (5), 3);
}

/*
** s64_x_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_x_ptrue_n ()
{
  return svdiv_n_s64_x (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** s64_z_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_z_ptrue_n ()
{
  return svdiv_n_s64_z (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** s64_m_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svint64_t s64_m_ptrue_n ()
{
  return svdiv_n_s64_m (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** s32_m_ptrue_dupq:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint32_t s32_m_ptrue_dupq ()
{
  return svdiv_s32_m (svptrue_b32 (), svdupq_s32 (3, 0, -5, 11),
		      svdupq_s32 (4, 1, -6, 0));
}

/*
** s32_z_ptrue_dupq:
**	mov	z[0-9]+\.s, #-2
**	ret
*/
svint32_t s32_z_ptrue_dupq ()
{
  return svdiv_s32_z (svptrue_b32 (), svdupq_s32 (6, -30, 100, -4),
		      svdupq_s32 (-3, 15, -50, 2));
}

/*
** u64_x_pg:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_x_pg (svbool_t pg)
{
  return svdiv_x (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_z_pg:
**	mov	z[0-9]+\.d, p[0-7]/z, #1
**	ret
*/
svuint64_t u64_z_pg (svbool_t pg)
{
  return svdiv_z (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_m_pg:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	udiv	\2, p[0-7]/m, \2, \1
**	ret
*/
svuint64_t u64_m_pg (svbool_t pg)
{
  return svdiv_m (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_x_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_x_ptrue ()
{
  return svdiv_x (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_z_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_z_ptrue ()
{
  return svdiv_z (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_m_ptrue:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_m_ptrue ()
{
  return svdiv_m (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_x_pg_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_x_pg_n (svbool_t pg)
{
  return svdiv_n_u64_x (pg, svdup_u64 (5), 3);
}

/*
** u64_z_pg_n:
**	mov	z[0-9]+\.d, p[0-7]/z, #1
**	ret
*/
svuint64_t u64_z_pg_n (svbool_t pg)
{
  return svdiv_n_u64_z (pg, svdup_u64 (5), 3);
}

/*
** u64_m_pg_n:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	udiv	\2, p[0-7]/m, \2, \1
**	ret
*/
svuint64_t u64_m_pg_n (svbool_t pg)
{
  return svdiv_n_u64_m (pg, svdup_u64 (5), 3);
}

/*
** u64_x_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_x_ptrue_n ()
{
  return svdiv_n_u64_x (svptrue_b64 (), svdup_u64 (5), 3);
}

/*
** u64_z_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_z_ptrue_n ()
{
  return svdiv_n_u64_z (svptrue_b64 (), svdup_u64 (5), 3);
}

/*
** u64_m_ptrue_n:
**	mov	z[0-9]+\.d, #1
**	ret
*/
svuint64_t u64_m_ptrue_n ()
{
  return svdiv_n_u64_m (svptrue_b64 (), svdup_u64 (5), 3);
}
