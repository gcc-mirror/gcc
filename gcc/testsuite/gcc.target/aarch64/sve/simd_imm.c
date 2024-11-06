/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** t1:
**	and	z[0-9]+.s, z[0-9]+.s, #?3
**	ret
*/
uint32x2_t t1 (uint32x2_t a)
{
  return vand_u32 (a, vdup_n_u32 (3));
}

/*
** t2:
**	orr	z[0-9]+.s, z[0-9]+.s, #?-3
**	ret
*/
uint32x2_t t2 (uint32x2_t a)
{
  return vorr_u32 (a, vdup_n_u32 (~2));
}

/*
** t3:
**	eor	z[0-9]+.s, z[0-9]+.s, #?3
**	ret
*/
uint32x2_t t3 (uint32x2_t a)
{
  return veor_u32 (a, vdup_n_u32 (3));
}
