/* { dg-do assemble } */
/* { dg-require-effective-target large_long_double } */
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
long double
fpr_to_fpr (long double q0, long double q1) [[arm::streaming_compatible]]
{
  return q1;
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
long double
gpr_to_fpr () [[arm::streaming_compatible]]
{
  register long double x0 asm ("x0");
  asm volatile ("" : "=r" (x0));
  return x0;
}

/*
** zero_to_fpr:
**	fmov	s0, wzr
**	ret
*/
long double
zero_to_fpr () [[arm::streaming_compatible]]
{
  return 0;
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
void
fpr_to_gpr (long double q0) [[arm::streaming_compatible]]
{
  register long double x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}
