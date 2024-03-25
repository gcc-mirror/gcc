/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nothing+simd"

/*
** fpr_to_fpr:
**	fmov	s0, s1
**	ret
*/
_Float16
fpr_to_fpr (_Float16 q0, _Float16 q1) [[arm::streaming_compatible]]
{
  return q1;
}

/*
** gpr_to_fpr:
**	fmov	s0, w0
**	ret
*/
_Float16
gpr_to_fpr () [[arm::streaming_compatible]]
{
  register _Float16 w0 asm ("w0");
  asm volatile ("" : "=r" (w0));
  return w0;
}

/*
** zero_to_fpr:
**	fmov	s0, wzr
**	ret
*/
_Float16
zero_to_fpr () [[arm::streaming_compatible]]
{
  return 0;
}

/*
** fpr_to_gpr:
**	fmov	w0, s0
**	ret
*/
void
fpr_to_gpr (_Float16 q0) [[arm::streaming_compatible]]
{
  register _Float16 w0 asm ("w0");
  w0 = q0;
  asm volatile ("" :: "r" (w0));
}
