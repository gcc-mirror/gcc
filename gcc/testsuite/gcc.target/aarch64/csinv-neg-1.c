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
  unsigned t = a ? b : ~c;
  return t;
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
  unsigned t = a ? b : d;
  return t;
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
  unsigned t = a ? 0 : ~b; 
  return t;
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
  unsigned t = a ? ~b : 0; 
  return t;
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
  unsigned t = a ? ~b : c; 
  return t;
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
  unsigned t = a ? d : c; 
  return t;
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
  unsigned t = a ? b : -c; 
  return t;
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
  unsigned t = a ? -b : c; 
  return t;
}

/* { dg-final { check-function-bodies "**" "" "" } } */
