/* { dg-do compile } */
/* { dg-options "-O -march=armv8.2-a+sve" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef long v2di  __attribute__((vector_size (16)));
typedef unsigned long v2udi  __attribute__((vector_size (16)));
typedef int v4si  __attribute__((vector_size (16)));
typedef unsigned int v4usi  __attribute__((vector_size (16)));

/*
** food:
**	ptrue	(p[0-7]).b, all
**	sdiv	z0.d, \1/m, z0.d, z1.d
**	ret
*/

v2di
food (v2di a, v2di b)
{
  return a / b;
}

/*
** fooud:
**	ptrue	(p[0-7]).b, all
**	udiv	z0.d, \1/m, z0.d, z1.d
**	ret
*/

v2udi
fooud (v2udi a, v2udi b)
{
  return a / b;
}

/*
** foos:
**	ptrue	(p[0-7]).b, all
**	sdiv	z0.s, \1/m, z0.s, z1.s
**	ret
*/

v4si
foos (v4si a, v4si b)
{
  return a / b;
}

/*
** foous:
**	ptrue	(p[0-7]).b, all
**	udiv	z0.s, \1/m, z0.s, z1.s
**	ret
*/

v4usi
foous (v4usi a, v4usi b)
{
  return a / b;
}

