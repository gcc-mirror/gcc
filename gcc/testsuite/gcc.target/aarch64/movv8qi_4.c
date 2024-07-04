/* { dg-do assemble } */
/* { dg-options "-O -mtune=neoverse-v1 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nosve"

typedef unsigned char v8qi __attribute__((vector_size(8)));

/*
** fpr_to_fpr:
**	fmov	d0, d1
**	ret
*/
v8qi
fpr_to_fpr (v8qi q0, v8qi q1) [[arm::streaming_compatible]]
{
  return q1;
}

/*
** gpr_to_fpr:
**	fmov	d0, x0
**	ret
*/
v8qi
gpr_to_fpr () [[arm::streaming_compatible]]
{
  register v8qi x0 asm ("x0");
  asm volatile ("" : "=r" (x0));
  return x0;
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
v8qi
zero_to_fpr () [[arm::streaming_compatible]]
{
  return (v8qi) {};
}

/*
** fpr_to_gpr:
**	umov	x0, v0\.d\[0\]
**	ret
*/
void
fpr_to_gpr (v8qi q0) [[arm::streaming_compatible]]
{
  register v8qi x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}
