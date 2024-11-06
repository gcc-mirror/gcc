/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f_qi:
**	ldr	b([0-9]+), \[x0\]
**	cnt	v\1.8b, v\1.8b
**	smov	w0, v\1.b\[0\]
**	ret
*/
unsigned
f_qi (unsigned char *a)
{
  return __builtin_popcountg (a[0]);
}

/*
** f_hi:
**	ldr	h([0-9]+), \[x0\]
**	ptrue	(p[0-7]).b, vl8
**	cnt	z\1.h, \2/m, z\1.h
**	smov	w0, v\1.h\[0\]
**	ret
*/
unsigned
f_hi (unsigned short *a)
{
  return __builtin_popcountg (a[0]);
}

/*
** f_si:
**	ldr	s([0-9]+), \[x0\]
**	ptrue	(p[0-7]).b, vl8
**	cnt	z\1.s, \2/m, z\1.s
**	umov	x0, v\1.d\[0\]
**	ret
*/
unsigned
f_si (unsigned int *a)
{
  return __builtin_popcountg (a[0]);
}

/*
** f_di:
**	ldr	d([0-9]+), \[x0\]
**	ptrue	(p[0-7])\.b, vl8
**	cnt	z\1\.d, \2/m, z\1\.d
**	fmov	x0, d\1
**	ret
*/
unsigned
f_di (unsigned long *a)
{
  return __builtin_popcountg (a[0]);
}
