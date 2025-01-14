/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
**t0:
**	ori	(\$r[0-9]+),\$r4,257
**	slli.d	\$r4,\1,11
**	jr	\$r1
*/
long
t0 (long x)
{
  return (x | 0x101) << 11;
}

/*
**t1:
**	xori	(\$r[0-9]+),\$r4,257
**	alsl.d	\$r4,\1,\$r5,3
**	jr	\$r1
*/
long
t1 (long x, long y)
{
  return ((x ^ 0x101) << 3) + y;
}

/*
**t2:
**	bstrins.d	(\$r[0-9]+),\$r0,15,4
**	alsl.d	\$r4,\1,\$r5,2
**	jr	\$r1
*/
long
t2 (long x, long y)
{
  return ((x & ~0xfff0) << 2) + y;
}

/*
**t3:
**	ori	(\$r[0-9]+),\$r4,3855
**	alsl.w	\$r4,\1,\$r5,1
**	jr	\$r1
*/
long
t3 (long x, long y)
{
  return (int)(((x | 0xf0f) << 1) + y);
}

/*
**t4:
**	bstrpick.d	(\$r[0-9]+),\$r4,31,0
**	slli.d	\$r4,\1,1
**	jr	\$r1
*/
unsigned long
t4 (unsigned long x)
{
  return x << 32 >> 31;
}

/*
**t5:
**	bstrpick.d	(\$r[0-9]+),\$r4,31,0
**	alsl.d	\$r4,\1,\$r5,2
**	jr	\$r1
*/
unsigned long
t5 (unsigned long x, unsigned long y)
{
  return (x << 32 >> 30) + y;
}

/*
**t6:
**	alsl.w	\$r4,\$r4,\$r5,2
**	jr	\$r1
*/
unsigned int
t6 (unsigned long x, unsigned long y)
{
  return (x << 32 >> 30) + y;
}

/*
**t7:
**	bstrins.d	\$r4,\$r0,47,0
**	alsl.d	\$r4,\$r4,\$r5,2
**	jr	\$r1
*/
unsigned long
t7 (unsigned long x, unsigned long y)
{
  return ((x & 0xffff000000000000) << 2) + y;
}
