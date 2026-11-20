/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

/*
** f1:
**	movk	w0, #0x1234, lsl #?16
**	ret
*/
uint32_t
f1 (uint32_t x)
{
  return (x & 0xffff) ^ 0x12340000;
}

/*
** f2:
**	movk	w0, #0x9876, lsl #?16
**	ret
*/
uint32_t
f2 (uint32_t x)
{
  return (x & 0xffff) | 0x98760000;
}

/*
** f3:
**	movk	w0, #0x100, lsl #?16
**	ret
*/
uint32_t
f3 (uint32_t x)
{
  return (x & 0xffff) | 0x01000000;
}

/*
** g1:
**	movk	x0, #0x1234, lsl #?48
**	ret
*/
uint64_t
g1 (uint64_t x)
{
  return (x & 0xffffffffffff) ^ 0x1234000000000000;
}
