/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include "arm_sve.h"

/*
** s64_x_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_op1 (svbool_t pg, svint64_t op2)
{
  return svmul_x (pg, svdup_s64 (0), op2);
}

/*
** s64_z_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_op1 (svbool_t pg, svint64_t op2)
{
  return svmul_z (pg, svdup_s64 (0), op2);
}

/*
** s64_m_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_m_pg_op1 (svbool_t pg, svint64_t op2)
{
  return svmul_m (pg, svdup_s64 (0), op2);
}

/*
** s64_x_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_ptrue_op1 (svint64_t op2)
{
  return svmul_x (svptrue_b64 (), svdup_s64 (0), op2);
}

/*
** s64_z_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_ptrue_op1 (svint64_t op2)
{
  return svmul_z (svptrue_b64 (), svdup_s64 (0), op2);
}

/*
** s64_m_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_m_ptrue_op1 (svint64_t op2)
{
  return svmul_m (svptrue_b64 (), svdup_s64 (0), op2);
}

/*
** s64_x_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_x (pg, op1, svdup_s64 (0));
}

/*
** s64_z_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_z (pg, op1, svdup_s64 (0));
}

/*
** s64_m_pg_op2:
**	mov	z[0-9]+\.d, p0/m, #0
**	ret
*/
svint64_t s64_m_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_m (pg, op1, svdup_s64 (0));
}

/*
** s64_x_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_ptrue_op2 (svint64_t op1)
{
  return svmul_x (svptrue_b64 (), op1, svdup_s64 (0));
}

/*
** s64_z_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_z_ptrue_op2 (svint64_t op1)
{
  return svmul_z (svptrue_b64 (), op1, svdup_s64 (0));
}

/*
** s64_m_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_m_ptrue_op2 (svint64_t op1)
{
  return svmul_m (svptrue_b64 (), op1, svdup_s64 (0));
}

/*
** s64_n_x_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_n_x_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_n_s64_x (pg, op1, 0);
}

/*
** s64_n_z_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_n_z_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_n_s64_z (pg, op1, 0);
}

/*
** s64_n_m_pg_op2:
**	mov	z[0-9]+\.d, p0/m, #0
**	ret
*/
svint64_t s64_n_m_pg_op2 (svbool_t pg, svint64_t op1)
{
  return svmul_n_s64_m (pg, op1, 0);
}

/*
** s64_n_x_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_n_x_ptrue_op2 (svint64_t op1)
{
  return svmul_n_s64_x (svptrue_b64 (), op1, 0);
}

/*
** s64_n_z_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_n_z_ptrue_op2 (svint64_t op1)
{
  return svmul_n_s64_z (svptrue_b64 (), op1, 0);
}

/*
** s64_n_m_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_n_m_ptrue_op2 (svint64_t op1)
{
  return svmul_n_s64_m (svptrue_b64 (), op1, 0);
}

/*
** u64_x_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_x_pg_op1 (svbool_t pg, svuint64_t op2)
{
  return svmul_x (pg, svdup_u64 (0), op2);
}

/*
** u64_z_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_z_pg_op1 (svbool_t pg, svuint64_t op2)
{
  return svmul_z (pg, svdup_u64 (0), op2);
}

/*
** u64_m_pg_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_m_pg_op1 (svbool_t pg, svuint64_t op2)
{
  return svmul_m (pg, svdup_u64 (0), op2);
}

/*
** u64_x_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_x_ptrue_op1 (svuint64_t op2)
{
  return svmul_x (svptrue_b64 (), svdup_u64 (0), op2);
}

/*
** u64_z_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_z_ptrue_op1 (svuint64_t op2)
{
  return svmul_z (svptrue_b64 (), svdup_u64 (0), op2);
}

/*
** u64_m_ptrue_op1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_m_ptrue_op1 (svuint64_t op2)
{
  return svmul_m (svptrue_b64 (), svdup_u64 (0), op2);
}

/*
** u64_x_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_x_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_x (pg, op1, svdup_u64 (0));
}

/*
** u64_z_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_z_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_z (pg, op1, svdup_u64 (0));
}

/*
** u64_m_pg_op2:
**	mov	z[0-9]+\.d, p0/m, #0
**	ret
*/
svuint64_t u64_m_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_m (pg, op1, svdup_u64 (0));
}

/*
** u64_x_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_x_ptrue_op2 (svuint64_t op1)
{
  return svmul_x (svptrue_b64 (), op1, svdup_u64 (0));
}

/*
** u64_z_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_z_ptrue_op2 (svuint64_t op1)
{
  return svmul_z (svptrue_b64 (), op1, svdup_u64 (0));
}

/*
** u64_m_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_m_ptrue_op2 (svuint64_t op1)
{
  return svmul_m (svptrue_b64 (), op1, svdup_u64 (0));
}

/*
** u64_n_x_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_n_x_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_n_u64_x (pg, op1, 0);
}

/*
** u64_n_z_pg_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_n_z_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_n_u64_z (pg, op1, 0);
}

/*
** u64_n_m_pg_op2:
**	mov	z[0-9]+\.d, p0/m, #0
**	ret
*/
svuint64_t u64_n_m_pg_op2 (svbool_t pg, svuint64_t op1)
{
  return svmul_n_u64_m (pg, op1, 0);
}

/*
** u64_n_x_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_n_x_ptrue_op2 (svuint64_t op1)
{
  return svmul_n_u64_x (svptrue_b64 (), op1, 0);
}

/*
** u64_n_z_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_n_z_ptrue_op2 (svuint64_t op1)
{
  return svmul_n_u64_z (svptrue_b64 (), op1, 0);
}

/*
** u64_n_m_ptrue_op2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svuint64_t u64_n_m_ptrue_op2 (svuint64_t op1)
{
  return svmul_n_u64_m (svptrue_b64 (), op1, 0);
}

