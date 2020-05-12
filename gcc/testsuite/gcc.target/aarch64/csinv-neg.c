/* { dg-do compile } */
/* { dg-options "-O2" } */

/*
** inv1:
**	cmp	w0, 0
**	csinv	w0, w1, w2, ne
**	ret
*/
unsigned long long
inv1(unsigned a, unsigned b, unsigned c)
{
  return a ? b : ~c;
}

/*
** inv1_local:
**	cmp	w0, 0
**	csinv	w0, w1, w2, ne
**	ret
*/
unsigned long long
inv1_local(unsigned a, unsigned b, unsigned c)
{
  unsigned d = ~c;
  return a ? b : d;
}

/*
** inv_zero1:
**	cmp	w0, 0
**	csinv	w0, wzr, w1, ne
**	ret
*/
unsigned long long
inv_zero1(unsigned a, unsigned b)
{
  return a ? 0 : ~b;
}

/*
** inv_zero2:
**	cmp	w0, 0
**	csinv	w0, wzr, w1, eq
**	ret
*/
unsigned long long
inv_zero2(unsigned a, unsigned b)
{
  return a ? ~b : 0;
}


/*
** inv2:
**	cmp	w0, 0
**	csinv	w0, w2, w1, eq
**	ret
*/
unsigned long long
inv2(unsigned a, unsigned b, unsigned c)
{
  return a ? ~b : c;
}

/*
** inv2_local:
**	cmp	w0, 0
**	csinv	w0, w2, w1, eq
**	ret
*/
unsigned long long
inv2_local(unsigned a, unsigned b, unsigned c)
{
  unsigned d = ~b;
  return a ? d : c;
}

/*
** neg1:
**	cmp	w0, 0
**	csneg	w0, w1, w2, ne
**	ret
*/
unsigned long long
neg1(unsigned a, unsigned b, unsigned c)
{
  return a ? b : -c;
}


/*
** neg2:
**	cmp	w0, 0
**	csneg	w0, w2, w1, eq
**	ret
*/
unsigned long long
neg2(unsigned a, unsigned b, unsigned c)
{
  return a ? -b : c;
}

/* { dg-final { check-function-bodies "**" "" "" } } */
