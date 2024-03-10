/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** foo1:
**	srsra	v0\.16b, v1\.16b, 3
**	ret
*/

int8x16_t
foo1 (int8x16_t acc, int8x16_t a)
{
  return vaddq_s8 (acc, vrshrq_n_s8 (a, 3));
}

/*
** foo2:
**	srshr	v0\.16b, v1\.16b, 3
**	ret
*/

int8x16_t
foo2 (int8x16_t acc, int8x16_t a)
{
  int8x16_t z = vdupq_n_s8 (0);
  return vrsraq_n_s8 (z, a, 3);
}

/*
** foo3:
**	ursra	v0\.16b, v1\.16b, 3
**	ret
*/

uint8x16_t
foo3 (uint8x16_t acc, uint8x16_t a)
{
  return vaddq_u8 (acc, vrshrq_n_u8 (a, 3));
}

/*
** foo4:
**	urshr	v0\.16b, v1\.16b, 3
**	ret
*/

uint8x16_t
foo4 (uint8x16_t acc, uint8x16_t a)
{
  uint8x16_t z = vdupq_n_u8 (0);
  return vrsraq_n_u8 (z, a, 3);
}

