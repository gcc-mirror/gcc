/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve -fno-vect-cost-model -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f_v4hi:
**	ptrue	(p[0-7]).b, vl8
**	ldr	d([0-9]+), \[x0\]
**	cnt	z\2.h, \1/m, z\2.h
**	str	d\2, \[x1\]
**	ret
*/
void
f_v4hi (unsigned short *__restrict b, unsigned short *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
  d[2] = __builtin_popcount (b[2]);
  d[3] = __builtin_popcount (b[3]);
}

/*
** f_v8hi:
**	ptrue	(p[0-7]).b, vl16
**	ldr	q([0-9]+), \[x0\]
**	cnt	z\2.h, \1/m, z\2.h
**	str	q\2, \[x1\]
**	ret
*/
void
f_v8hi (unsigned short *__restrict b, unsigned short *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
  d[2] = __builtin_popcount (b[2]);
  d[3] = __builtin_popcount (b[3]);
  d[4] = __builtin_popcount (b[4]);
  d[5] = __builtin_popcount (b[5]);
  d[6] = __builtin_popcount (b[6]);
  d[7] = __builtin_popcount (b[7]);
}

/*
** f_v2si:
**	ptrue	(p[0-7]).b, vl8
**	ldr	d([0-9]+), \[x0\]
**	cnt	z\2.s, \1/m, z\2.s
**	str	d\2, \[x1\]
**	ret
*/
void
f_v2si (unsigned int *__restrict b, unsigned int *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
}

/*
** f_v4si:
**	ptrue	(p[0-7]).b, vl16
**	ldr	q([0-9]+), \[x0\]
**	cnt	z\2.s, \1/m, z\2.s
**	str	q\2, \[x1\]
**	ret
*/
void
f_v4si (unsigned int *__restrict b, unsigned int *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
  d[2] = __builtin_popcount (b[2]);
  d[3] = __builtin_popcount (b[3]);
}

/*
** f_v2di:
**	ptrue	(p[0-7]).b, vl16
**	ldr	q([0-9]+), \[x0\]
**	cnt	z\2.d, \1/m, z\2.d
**	str	q\2, \[x1\]
**	ret
*/
void
f_v2di (unsigned long *__restrict b, unsigned long *__restrict d)
{
  d[0] = __builtin_popcountll (b[0]);
  d[1] = __builtin_popcountll (b[1]);
}
