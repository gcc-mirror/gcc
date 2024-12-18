/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve+sve2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** lsl_usra_32_sve_lsl_imm:
**	lsl	z0.d, z1.d, #34
**	usra	z0.d, z1.d, #30
**	ret
*/
svuint64_t
lsl_usra_32_sve_lsl_imm (svuint64_t __attribute__ ((unused)) dummy, svuint64_t r)
{
  return svorr_u64_z (svptrue_b64 (), svlsl_n_u64_z (svptrue_b64 (), r, 34),
		      svlsr_n_u64_z (svptrue_b64 (), r, 30));
}

/*
** lsl_usra_32_sve_lsl_operand:
**	lsl	z0.d, z1.d, #34
**	usra	z0.d, z1.d, #30
**	ret
*/
svuint64_t
lsl_usra_32_sve_lsl_operand (svuint64_t __attribute__ ((unused)) dummy, svuint64_t r)
{
  svbool_t pt = svptrue_b64 ();
  return svorr_u64_z (pt, svlsl_n_u64_z (pt, r, 34), svlsr_n_u64_z (pt, r, 30));
}

/*
** lsl_usra_16_sve_lsl_imm:
**	lsl	z0.s, z1.s, #14
**	usra	z0.s, z1.s, #18
**	ret
*/
svuint32_t
lsl_usra_16_sve_lsl_imm (svuint32_t __attribute__ ((unused)) dummy, svuint32_t r)
{
  return svorr_u32_z (svptrue_b32 (), svlsl_n_u32_z (svptrue_b32 (), r, 14),
		      svlsr_n_u32_z (svptrue_b32 (), r, 18));
}

/*
** lsl_usra_16_sve_lsl_operand:
**	lsl	z0.s, z1.s, #14
**	usra	z0.s, z1.s, #18
**	ret
*/
svuint32_t
lsl_usra_16_sve_lsl_operand (svuint32_t __attribute__ ((unused)) dummy, svuint32_t r)
{
  svbool_t pt = svptrue_b32 ();
  return svorr_u32_z (pt, svlsl_n_u32_z (pt, r, 14), svlsr_n_u32_z (pt, r, 18));
}

/*
** lsl_usra_8_sve_lsl_imm:
**	lsl	z0.h, z1.h, #6
**	usra	z0.h, z1.h, #10
**	ret
*/
svuint16_t
lsl_usra_8_sve_lsl_imm (svuint16_t __attribute__ ((unused)) dummy, svuint16_t r)
{
  return svorr_u16_z (svptrue_b16 (), svlsl_n_u16_z (svptrue_b16 (), r, 6),
		      svlsr_n_u16_z (svptrue_b16 (), r, 10));
}

/*
** lsl_usra_8_sve_lsl_operand:
**	lsl	z0.h, z1.h, #6
**	usra	z0.h, z1.h, #10
**	ret
*/
svuint16_t
lsl_usra_8_sve_lsl_operand (svuint16_t __attribute__ ((unused)) dummy, svuint16_t r)
{
  svbool_t pt = svptrue_b16 ();
  return svorr_u16_z (pt, svlsl_n_u16_z (pt, r, 6), svlsr_n_u16_z (pt, r, 10));
}
