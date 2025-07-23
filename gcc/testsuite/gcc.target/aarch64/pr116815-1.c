/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* PR middle-end/116815 */

/* Single-use tests.  */

static inline unsigned __attribute__ ((always_inline))
max (unsigned a, unsigned b)
{
  return a > b ? a : b;
}

static inline unsigned __attribute__ ((always_inline))
min (unsigned a, unsigned b)
{
  return a < b ? a : b;
}

#define OPERATION(op, type, N, exp1, exp2)                                     \
  unsigned u##op##type##N (unsigned a, unsigned b) { return op (exp1, exp2); }

/*
** umaxadd1:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, add, 1, a, a + b)

/*
** umaxadd2:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, add, 2, a, b + a)

/*
** umaxadd3:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, add, 3, a + b, a)

/*
** umaxadd4:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, add, 4, b + a, a)

/*
** uminadd1:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, add, 1, a, a + b)

/*
** uminadd2:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, add, 2, a, b + a)

/*
** uminadd3:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, add, 3, a + b, a)

/*
** uminadd4:
**	adds	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, add, 4, b + a, a)

/* sub requires the inverse of the comparison from add.  */

/*
** umaxsub1:
**	subs	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, sub, 1, a, a - b)

/*
** umaxsub2:
**	subs	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cc
**	ret
*/
OPERATION (max, sub, 2, a - b, a)

/*
** uminsub1:
**	subs	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, sub, 1, a, a - b)

/*
** uminsub2:
**	subs	(w[0-9]+), w0, w1
**	csel	w0, \1, w0, cs
**	ret
*/
OPERATION (min, sub, 2, a - b, a)
