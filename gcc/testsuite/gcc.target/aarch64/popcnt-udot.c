/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+dotprod -fno-vect-cost-model -fno-schedule-insns -fno-schedule-insns2" } */

/*
** bar:
**	movi	v([0-9]+).16b, 0x1
**	movi	v([0-9]+).4s, 0
**	ldr	q([0-9]+), \[x0\]
**	cnt	v([0-9]+).16b, v\3.16b
**	udot	v\2.4s, v\4.16b, v\1.16b
**	str	q\2, \[x1\]
**	ret
*/
void
bar (unsigned int *__restrict b, unsigned int *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
  d[2] = __builtin_popcount (b[2]);
  d[3] = __builtin_popcount (b[3]);
}

/*
** bar1:
**	movi	v([0-9]+).8b, 0x1
**	movi	v([0-9]+).2s, 0
**	ldr	d([0-9]+), \[x0\]
**	cnt	v([0-9]+).8b, v\3.8b
**	udot	v\2.2s, v\4.8b, v\1.8b
**	str	d\2, \[x1\]
**	ret
*/
void
bar1 (unsigned int *__restrict b, unsigned int *__restrict d)
{
  d[0] = __builtin_popcount (b[0]);
  d[1] = __builtin_popcount (b[1]);
}

/*
** bar2:
**	movi	v([0-9]+).16b, 0x1
**	movi	v([0-9]+).4s, 0
**	ldr	q([0-9]+), \[x0\]
**	cnt	v([0-9]+).16b, v\3.16b
**	udot	v\2.4s, v\4.16b, v\1.16b
**	uaddlp	v\2.2d, v\2.4s
**	str	q\2, \[x1\]
**	ret
*/
void
bar2 (unsigned long long *__restrict b, unsigned long long *__restrict d)
{
  d[0] = __builtin_popcountll (b[0]);
  d[1] = __builtin_popcountll (b[1]);
}

/* { dg-final { check-function-bodies "**" "" "" } } */
