/* { dg-do compile { target float16 } } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** copysign_reg:
**	cpsdr	%f0,%f2,%f0
**	br	%r14
*/

_Float16
copysign_reg (_Float16 x, _Float16 y)
{
  return __builtin_copysignf16 (x, y);
}

/*
** copysign_mem:
**	vleh	%v([0-9]+),0\(%r2\),0
**	vleh	%v([0-9]+),0\(%r3\),0
**	cpsdr	%f0,%f\2,%f\1
**	br	%r14
*/

_Float16
copysign_mem (_Float16 *x, _Float16 *y)
{
  return __builtin_copysignf16 (*x, *y);
}
