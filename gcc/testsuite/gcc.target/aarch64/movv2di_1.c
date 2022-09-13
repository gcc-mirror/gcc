/* { dg-do assemble } */
/* { dg-options "-O -mtune=neoverse-v1 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nothing+nosimd+fp"

typedef long long v2di __attribute__((vector_size(16)));

/*
** fpr_to_fpr:
**	sub	sp, sp, #16
**	str	q1, \[sp\]
**	ldr	q0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
v2di
fpr_to_fpr (v2di q0, v2di q1)
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
v2di
gpr_to_fpr ()
{
  register v2di x0 asm ("x0");
  asm volatile ("" : "=r" (x0));
  return x0;
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
v2di
zero_to_fpr ()
{
  return (v2di) {};
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
fpr_to_gpr (v2di q0)
{
  register v2di x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}

/*
** gpr_to_gpr:
** (
**	mov	x0, x2
**	mov	x1, x3
** |
**	mov	x1, x3
**	mov	x0, x2
** )
**	ret
*/
void
gpr_to_gpr ()
{
  register v2di x0 asm ("x0");
  register v2di x2 asm ("x2");
  asm volatile ("" : "=r" (x2));
  x0 = x2;
  asm volatile ("" :: "r" (x0));
}
