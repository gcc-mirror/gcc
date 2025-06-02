/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** ror32_sve_lsl_imm:
**	ptrue	(p[0-3]).b, all
**	revw	z0.d, \1/m, z0.d
**	ret
*/
svuint64_t
ror32_sve_lsl_imm (svuint64_t r)
{
  return svorr_u64_z (svptrue_b64 (), svlsl_n_u64_z (svptrue_b64 (), r, 32),
		      svlsr_n_u64_z (svptrue_b64 (), r, 32));
}

/*
** ror32_sve_lsl_operand:
**	ptrue	(p[0-3]).b, all
**	revw	z0.d, \1/m, z0.d
**	ret
*/
svuint64_t
ror32_sve_lsl_operand (svuint64_t r)
{
  svbool_t pt = svptrue_b64 ();
  return svorr_u64_z (pt, svlsl_n_u64_z (pt, r, 32), svlsr_n_u64_z (pt, r, 32));
}

/*
** ror16_sve_lsl_imm:
**	ptrue	(p[0-3]).b, all
**	revh	z0.s, \1/m, z0.s
**	ret
*/
svuint32_t
ror16_sve_lsl_imm (svuint32_t r)
{
  return svorr_u32_z (svptrue_b32 (), svlsl_n_u32_z (svptrue_b32 (), r, 16),
		      svlsr_n_u32_z (svptrue_b32 (), r, 16));
}

/*
** ror16_sve_lsl_operand:
**	ptrue	(p[0-3]).b, all
**	revh	z0.s, \1/m, z0.s
**	ret
*/
svuint32_t
ror16_sve_lsl_operand (svuint32_t r)
{
  svbool_t pt = svptrue_b32 ();
  return svorr_u32_z (pt, svlsl_n_u32_z (pt, r, 16), svlsr_n_u32_z (pt, r, 16));
}

/*
** ror8_sve_lsl_imm:
**	ptrue	(p[0-3]).b, all
**	revb	z0.h, \1/m, z0.h
**	ret
*/
svuint16_t
ror8_sve_lsl_imm (svuint16_t r)
{
  return svorr_u16_z (svptrue_b16 (), svlsl_n_u16_z (svptrue_b16 (), r, 8),
		      svlsr_n_u16_z (svptrue_b16 (), r, 8));
}

/*
** ror8_sve_lsl_operand:
**	ptrue	(p[0-3]).b, all
**	revb	z0.h, \1/m, z0.h
**	ret
*/
svuint16_t
ror8_sve_lsl_operand (svuint16_t r)
{
  svbool_t pt = svptrue_b16 ();
  return svorr_u16_z (pt, svlsl_n_u16_z (pt, r, 8), svlsr_n_u16_z (pt, r, 8));
}
