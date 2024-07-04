/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** fpr_to_fpr:
**	fmov	s0, s1
**	ret
*/
float
fpr_to_fpr (float q0, float q1) [[arm::streaming_compatible]]
{
  return q1;
}

/*
** gpr_to_fpr:
**	fmov	s0, w0
**	ret
*/
float
gpr_to_fpr () [[arm::streaming_compatible]]
{
  register float w0 asm ("w0");
  asm volatile ("" : "=r" (w0));
  return w0;
}

/*
** zero_to_fpr:
**	fmov	s0, wzr
**	ret
*/
float
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
fpr_to_gpr (float q0) [[arm::streaming_compatible]]
{
  register float w0 asm ("w0");
  w0 = q0;
  asm volatile ("" :: "r" (w0));
}
