/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** foo_8_32:
**	uaddlv	h0, v0.8b
**	str	s0, \[x0\]
**	ret
*/

void
foo_8_32 (uint8x8_t a, uint32_t *res)
{
  *res = vaddlv_u8 (a);
}

/*
** foo_8_64:
**	uaddlv	h0, v0.8b
**	str	d0, \[x0\]
**	ret
*/

void
foo_8_64 (uint8x8_t a, uint64_t *res)
{
  *res = vaddlv_u8 (a);
}

/*
** foo_16_64:
**	uaddlv	s0, v0.4h
**	str	d0, \[x0\]
**	ret
*/

void
foo_16_64 (uint16x4_t a, uint64_t *res)
{
  *res = vaddlv_u16 (a);
}

/*
** fooq_8_32:
**	uaddlv	h0, v0.16b
**	str	s0, \[x0\]
**	ret
*/

void
fooq_8_32 (uint8x16_t a, uint32_t *res)
{
  *res = vaddlvq_u8 (a);
}

/*
** fooq_8_64:
**	uaddlv	h0, v0.16b
**	str	d0, \[x0\]
**	ret
*/

void
fooq_8_64 (uint8x16_t a, uint64_t *res)
{
  *res = vaddlvq_u8 (a);
}

/*
** fooq_16_64:
**	uaddlv	s0, v0.8h
**	str	d0, \[x0\]
**	ret
*/

void
fooq_16_64 (uint16x8_t a, uint64_t *res)
{
  *res = vaddlvq_u16 (a);
}

