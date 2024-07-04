/* { dg-do assemble } */
/* { dg-options "-O -mtune=neoverse-v1 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nosve"

typedef unsigned char v16qi __attribute__((vector_size(16)));

/*
** fpr_to_fpr:
**	sub	sp, sp, #16
**	str	q1, \[sp\]
**	ldr	q0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
v16qi
fpr_to_fpr (v16qi q0, v16qi q1) [[arm::streaming_compatible]]
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
v16qi
gpr_to_fpr () [[arm::streaming_compatible]]
{
  register v16qi x0 asm ("x0");
  asm volatile ("" : "=r" (x0));
  return x0;
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
v16qi
zero_to_fpr () [[arm::streaming_compatible]]
{
  return (v16qi) {};
}

/*
** fpr_to_gpr:	{ target aarch64_little_endian }
** (
**	umov	x0, v0.d\[0\]
**	fmov	x1, v0.d\[1\]
** |
**	fmov	x1, v0.d\[1\]
**	umov	x0, v0.d\[0\]
** )
**	ret
*/
/*
** fpr_to_gpr:	{ target aarch64_big_endian }
** (
**	umov	x1, v0.d\[0\]
**	fmov	x0, v0.d\[1\]
** |
**	fmov	x0, v0.d\[1\]
**	umov	x1, v0.d\[0\]
** )
**	ret
*/
void
fpr_to_gpr (v16qi q0) [[arm::streaming_compatible]]
{
  register v16qi x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}
