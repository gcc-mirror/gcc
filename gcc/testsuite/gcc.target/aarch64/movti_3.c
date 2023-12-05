/* { dg-do assemble } */
/* { dg-options "-O -mtune=neoverse-v1 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nosve"

/*
** fpr_to_fpr:
**	sub	sp, sp, #16
**	str	q1, \[sp\]
**	ldr	q0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
void
fpr_to_fpr (void) [[arm::streaming_compatible]]
{
  register __int128_t q0 asm ("q0");
  register __int128_t q1 asm ("q1");
  asm volatile ("" : "=w" (q1));
  q0 = q1;
  asm volatile ("" :: "w" (q0));
}

/*
** gpr_to_fpr:	{ target aarch64_little_endian }
**	fmov	d0, x0
**	fmov	v0.d\[1\], x1
**	ret
*/
/*
** gpr_to_fpr:	{ target aarch64_big_endian }
**	fmov	d0, x1
**	fmov	v0.d\[1\], x0
**	ret
*/
void
gpr_to_fpr (__int128_t x0) [[arm::streaming_compatible]]
{
  register __int128_t q0 asm ("q0");
  q0 = x0;
  asm volatile ("" :: "w" (q0));
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
void
zero_to_fpr () [[arm::streaming_compatible]]
{
  register __int128_t q0 asm ("q0");
  q0 = 0;
  asm volatile ("" :: "w" (q0));
}

/*
** fpr_to_gpr:	{ target aarch64_little_endian }
** (
**	fmov	x0, d0
**	fmov	x1, v0.d\[1\]
** |
**	fmov	x1, v0.d\[1\]
**	fmov	x0, d0
** )
**	ret
*/
/*
** fpr_to_gpr:	{ target aarch64_big_endian }
** (
**	fmov	x1, d0
**	fmov	x0, v0.d\[1\]
** |
**	fmov	x0, v0.d\[1\]
**	fmov	x1, d0
** )
**	ret
*/
__int128_t
fpr_to_gpr () [[arm::streaming_compatible]]
{
  register __int128_t q0 asm ("q0");
  asm volatile ("" : "=w" (q0));
  return q0;
}
