/* { dg-do assemble } */
/* { dg-options "-O -mtune=neoverse-v1 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nothing+nosimd+fp"

typedef unsigned char v8qi __attribute__((vector_size(8)));

/*
** fpr_to_fpr:
**	fmov	d0, d1
**	ret
*/
v8qi
fpr_to_fpr (v8qi q0, v8qi q1)
{
  return q1;
}

/*
** gpr_to_fpr:
**	fmov	d0, x0
**	ret
*/
v8qi
gpr_to_fpr ()
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
zero_to_fpr ()
{
  return (v8qi) {};
}

/*
** fpr_to_gpr:
**	fmov	x0, d0
**	ret
*/
void
fpr_to_gpr (v8qi q0)
{
  register v8qi x0 asm ("x0");
  x0 = q0;
  asm volatile ("" :: "r" (x0));
}

/*
** gpr_to_gpr:
**	mov	x0, x1
**	ret
*/
void
gpr_to_gpr ()
{
  register v8qi x0 asm ("x0");
  register v8qi x1 asm ("x1");
  asm volatile ("" : "=r" (x1));
  x0 = x1;
  asm volatile ("" :: "r" (x0));
}
