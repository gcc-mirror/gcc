/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nothing+nosimd+fp"

/*
** fpr_to_fpr:
**	fmov	d0, d1
**	ret
*/
double
fpr_to_fpr (double q0, double q1)
{
  return q1;
}

/*
** gpr_to_fpr:
**	fmov	d0, x0
**	ret
*/
double
gpr_to_fpr ()
{
  register double x0 asm ("x0");
  asm volatile ("" : "=r" (x0));
  return x0;
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
double
zero_to_fpr ()
{
  return 0;
}

/*
** fpr_to_gpr:
**	fmov	x0, d0
**	ret
*/
void
fpr_to_gpr (double q0)
{
  register double x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}
