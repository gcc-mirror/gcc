/* { dg-do compile { target float16 } } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Calling Convention Tests */

/* Parameters of type _Float16 are passed via FPRs, if possible.  Return values
   are passed via FPR f2.  */

/*
** test_arg:
**	vlr	%v0,%v2
**	br	%r14
*/

_Float16
test_arg (double unused, _Float16 x)
{
  return x;
}

/* Test passing a struct with a single member.  */

struct s { _Float16 x; };

/*
** test:
**     br	%r14
*/

_Float16
test (struct s y)
{
  return y.x;
}

/* Test _Float16 _Complex which must be returned via reference.  */

/*
** test_complex_return:
**	...
**	ste	%[fv][0-9]+,0\(%r2\)
**	br	%r14
*/

_Float16 _Complex
test_complex_return (_Float16 a, _Float16 b)
{
  _Float16 _Complex x;
  __real__ x = a;
  __imag__ x = b;
  return x;
}

/* Likewise, an argument of type _Float16 _Complex is passed via reference.  */

/*
** test_complex_arg:
**	vleh	%v0,0\(%r2\),0
**	br	%r14
*/

_Float16
test_complex_arg (_Float16 _Complex x)
{
  return __real__ x;
}
