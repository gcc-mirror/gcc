/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** test_ldexpf16:
**	...
**	ptrue	(p[0-7]).b, vl2
**	...
**	fscale	z[0-9]+\.h, \1/m, z[0-9]+\.h, z[0-9]+\.h
**	ret
*/
_Float16
test_ldexpf16 (_Float16 x, int i)
{
  return __builtin_ldexpf16 (x, i);
}

/*
** test_ldexpf:
**	...
**	ptrue	(p[0-7])\.b, vl4
**	...
**	fscale	z[0-9]+\.s, \1/m, z[0-9]+\.s, z[0-9]+\.s
**	ret
*/
float
test_ldexpf (float x, int i)
{
  return __builtin_ldexpf (x, i);
}

/*
** test_ldexp:
**	...
**	ptrue	(p[0-7]).b, vl8
**	...
**	fscale	z[0-9]+\.d, \1/m, z[0-9]+\.d, z[0-9]+\.d
**	ret
*/
double
test_ldexp (double x, int i)
{
  return __builtin_ldexp (x, i);
} 

