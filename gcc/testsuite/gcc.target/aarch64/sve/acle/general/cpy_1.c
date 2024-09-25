/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** dup_x0_m:
**	...
** (
**	add	(x[0-9]+), x0, #?1
**	mov	(p[0-7])\.b, p15\.b
**	mov	z0\.d, \2/m, \1
** |
**	mov	(p[0-7])\.b, p15\.b
**	add	(x[0-9]+), x0, #?1
**	mov	z0\.d, \3/m, \4
** )
**	...
**	ret
*/
svuint64_t
dup_x0_m (svuint64_t z0, uint64_t x0)
{
  register svbool_t pg asm ("p15");
  asm volatile ("" : "=Upa" (pg));
  return svdup_u64_m (z0, pg, x0 + 1);
}

/*
** dup_d1_z:
**	...
**	mov	(p[0-7])\.b, p15\.b
**	mov	z0\.d, \1/m, d1
**	...
**	ret
*/
svfloat64_t
dup_d1_z (svfloat64_t z0, float64_t d1)
{
  register svbool_t pg asm ("p15");
  asm volatile ("" : "=Upa" (pg));
  return svdup_f64_m (z0, pg, d1);
}

#ifdef __cplusplus
}
#endif
