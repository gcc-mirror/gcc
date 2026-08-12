/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -march=armv8-a+sve -mautovec-preference=sve-only --param vect-epilogues-nomask=0 -fdump-tree-vect-details" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define DEF(NAME, INPUT, OUTPUT) \
  OUTPUT                        \
  NAME (const INPUT *a, long n) \
  {                             \
    OUTPUT sum = 0;             \
    for (long i = 0; i < n; ++i) \
      sum += a[i];              \
    return sum;                 \
  }

/*
** sum_u8_long:
**	...
**	cnth	x[0-9]+
**	...
**	ptrue	(p[0-7])\.b, all
**	mov	(z[0-9]+\.h), #1
**	...
**	ld1b	(z[0-9]+\.h), \1/z, \[[^]]*\]
**	...
**	udot	(z[0-9]+)\.d, \3, \2
**	...
**	uaddv	d[0-9]+, \1, \4\.d
**	...
*/
DEF (sum_u8_long, unsigned char, long)

/*
** sum_s8_long:
**	...
**	cnth	x[0-9]+
**	...
**	ptrue	(p[0-7])\.b, all
**	mov	(z[0-9]+\.h), #1
**	...
**	ld1sb	(z[0-9]+\.h), \1/z, \[[^]]*\]
**	...
**	sdot	(z[0-9]+)\.d, \3, \2
**	...
**	uaddv	d[0-9]+, \1, \4\.d
**	...
*/
DEF (sum_s8_long, signed char, long)

/*
** sum_u8_int:
**	...
**	cntb	x[0-9]+
**	...
**	ptrue	(p[0-7])\.b, all
**	mov	(z[0-9]+\.b), #1
**	...
**	ld1b	(z[0-9]+\.b), \1/z, \[[^]]*\]
**	...
**	udot	(z[0-9]+)\.s, \3, \2
**	...
**	uaddv	d[0-9]+, \1, \4\.s
**	...
*/
DEF (sum_u8_int, unsigned char, int)

/*
** sum_s8_int:
**	...
**	cntb	x[0-9]+
**	...
**	ptrue	(p[0-7])\.b, all
**	mov	(z[0-9]+\.b), #1
**	...
**	ld1b	(z[0-9]+\.b), \1/z, \[[^]]*\]
**	...
**	sdot	(z[0-9]+)\.s, \3, \2
**	...
**	uaddv	d[0-9]+, \1, \4\.s
**	...
*/
DEF (sum_s8_int, signed char, int)

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 4 "vect" } } */
