/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
**t1:
**	revb.2w	\$r4,\$r4
**	slli.w	\$r4,\$r4,0
**	jr	\$r1
*/
unsigned int
t1 (unsigned int x)
{
  return __builtin_bswap32 (x);
}

/*
**t2:
**	revb.d	\$r4,\$r4
**	jr	\$r1
*/
unsigned long
t2 (unsigned long x)
{
  return __builtin_bswap64 (x);
}

/*
**t3:
**	revb.2h	\$r4,\$r4
**	jr	\$r1
*/
unsigned int
t3 (unsigned int x)
{
  return (x >> 8) & 0xff00ff | (x << 8) & 0xff00ff00;
}

/*
**t4:
**	revb.2w	\$r4,\$r4
**	jr	\$r1
*/
unsigned long
t4 (unsigned long x)
{
  x = __builtin_bswap64 (x);
  return x << 32 | x >> 32;
}

/*
**t5:
**	revb.2h	\$r4,\$r4
**	bstrpick.w	\$r4,\$r4,15,0
**	jr	\$r1
*/
unsigned short
t5 (unsigned short x)
{
  return __builtin_bswap16 (x);
}
