/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

#define H3 ((uint64_t) 0xffff << 48)
#define H2 ((uint64_t) 0xffff << 32)
#define H1 ((uint64_t) 0xffff << 16)
#define H0 ((uint64_t) 0xffff)

/*
** f1:
**	mov	w0, w1
**	movk	w0, #0x9876(?:, lsl #?0)?
**	ret
*/
uint32_t
f1 (uint32_t dummy, uint32_t x)
{
  return (x & 0xffff0000) | 0x9876;
}

/*
** f2:
**	movk	w0, #0x1234, lsl #?16
**	ret
*/
uint32_t
f2 (uint32_t x)
{
  return (x & 0xffff) | 0x12340000;
}

/*
** g1:
**	movk	x0, #0x1234, lsl #?0
**	ret
*/
uint64_t
g1 (uint64_t x)
{
  return (x & (H3 | H2 | H1)) | 0x1234;
}

/*
** g2:
**	movk	x0, #0x900e, lsl #?16
**	ret
*/
uint64_t
g2 (uint64_t x)
{
  return (x & (H3 | H2 | H0)) | ((uint64_t) 0x900e << 16);
}

/*
** g3:
**	movk	x0, #0xee33, lsl #?32
**	ret
*/
uint64_t
g3 (uint64_t x)
{
  return (x & (H3 | H1 | H0)) | ((uint64_t) 0xee33 << 32);
}

/*
** g4:
**	mov	x0, x1
**	movk	x0, #0x7654, lsl #?48
**	ret
*/
uint64_t
g4 (uint64_t dummy, uint64_t x)
{
  return (x & (H2 | H1 | H0)) | ((uint64_t) 0x7654 << 48);
}
