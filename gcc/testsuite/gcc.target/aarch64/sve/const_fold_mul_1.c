/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include "arm_sve.h"

/*
** s64_x_pg:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_x_pg (svbool_t pg)
{
  return svmul_x (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_pg_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_0 (svbool_t pg)
{
  return svmul_x (pg, svdup_s64 (0), svdup_s64 (3));
}

/*
** s64_z_pg:
**	mov	z[0-9]+\.d, p[0-7]/z, #15
**	ret
*/
svint64_t s64_z_pg (svbool_t pg)
{
  return svmul_z (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_z_pg_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_0 (svbool_t pg)
{
  return svmul_z (pg, svdup_s64 (0), svdup_s64 (3));
}

/*
** s64_m_pg:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	mul	\2, p[0-7]/m, \2, \1
**	ret
*/
svint64_t s64_m_pg (svbool_t pg)
{
  return svmul_m (pg, svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_x_ptrue ()
{
  return svmul_x (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_z_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_z_ptrue ()
{
  return svmul_z (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_m_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_m_ptrue ()
{
  return svmul_m (svptrue_b64 (), svdup_s64 (5), svdup_s64 (3));
}

/*
** s64_x_pg_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_x_pg_n (svbool_t pg)
{
  return svmul_n_s64_x (pg, svdup_s64 (5), 3);
}

/*
** s64_x_pg_n_s64_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_n_s64_0 (svbool_t pg)
{
  return svmul_n_s64_x (pg, svdup_s64 (5), 0);
}

/*
** s64_z_pg_n:
**	mov	z[0-9]+\.d, p[0-7]/z, #15
**	ret
*/
svint64_t s64_z_pg_n (svbool_t pg)
{
  return svmul_n_s64_z (pg, svdup_s64 (5), 3);
}

/*
** s64_z_pg_n_s64_0:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_n_s64_0 (svbool_t pg)
{
  return svmul_n_s64_z (pg, svdup_s64 (5), 0);
}

/*
** s64_m_pg_n:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	mul	\2, p[0-7]/m, \2, \1
**	ret
*/
svint64_t s64_m_pg_n (svbool_t pg)
{
  return svmul_n_s64_m (pg, svdup_s64 (5), 3);
}

/*
** s64_x_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_x_ptrue_n ()
{
  return svmul_n_s64_x (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** s64_z_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_z_ptrue_n ()
{
  return svmul_n_s64_z (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** s64_m_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svint64_t s64_m_ptrue_n ()
{
  return svmul_n_s64_m (svptrue_b64 (), svdup_s64 (5), 3);
}

/*
** u64_x_pg:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_x_pg (svbool_t pg)
{
  return svmul_x (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_z_pg:
**	mov	z[0-9]+\.d, p[0-7]/z, #15
**	ret
*/
svuint64_t u64_z_pg (svbool_t pg)
{
  return svmul_z (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_m_pg:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	mul	\2, p[0-7]/m, \2, \1
**	ret
*/
svuint64_t u64_m_pg (svbool_t pg)
{
  return svmul_m (pg, svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_x_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_x_ptrue ()
{
  return svmul_x (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_z_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_z_ptrue ()
{
  return svmul_z (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_m_ptrue:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_m_ptrue ()
{
  return svmul_m (svptrue_b64 (), svdup_u64 (5), svdup_u64 (3));
}

/*
** u64_x_pg_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_x_pg_n (svbool_t pg)
{
  return svmul_n_u64_x (pg, svdup_u64 (5), 3);
}

/*
** u64_z_pg_n:
**	mov	z[0-9]+\.d, p[0-7]/z, #15
**	ret
*/
svuint64_t u64_z_pg_n (svbool_t pg)
{
  return svmul_n_u64_z (pg, svdup_u64 (5), 3);
}

/*
** u64_m_pg_n:
**	mov	(z[0-9]+\.d), #3
**	mov	(z[0-9]+\.d), #5
**	mul	\2, p[0-7]/m, \2, \1
**	ret
*/
svuint64_t u64_m_pg_n (svbool_t pg)
{
  return svmul_n_u64_m (pg, svdup_u64 (5), 3);
}

/*
** u64_x_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_x_ptrue_n ()
{
  return svmul_n_u64_x (svptrue_b64 (), svdup_u64 (5), 3);
}

/*
** u64_z_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_z_ptrue_n ()
{
  return svmul_n_u64_z (svptrue_b64 (), svdup_u64 (5), 3);
}

/*
** u64_m_ptrue_n:
**	mov	z[0-9]+\.d, #15
**	ret
*/
svuint64_t u64_m_ptrue_n ()
{
  return svmul_n_u64_m (svptrue_b64 (), svdup_u64 (5), 3);
}

/*
** u32_x_pg:
**	mov	z[0-9]+\.s, #60
**	ret
*/
svuint32_t u32_x_pg (svbool_t pg)
{
  return svmul_x (pg, svdupq_u32 (3, 15, 1, 12), svdupq_u32 (20, 4, 60, 5));
}
