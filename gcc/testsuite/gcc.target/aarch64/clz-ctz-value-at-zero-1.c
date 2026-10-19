/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* 32 is the value of CLZ at zero for SImode, so the CLZ instruction
   gives the result.  */

/*
** clz32:
**	clz	w0, w0
**	ret
*/
int __GIMPLE (ssa)
clz32 (unsigned int x)
{
  int _1;

  __BB(2):
  _1 = .CLZ (x_2(D), 32);
  return _1;
}

/* Another value at zero needs a test of the argument.  */

/*
** clz5:
**	clz	(w[0-9]+), w0
**	cmp	w0, 0
**	mov	(w[0-9]+), 5
**	csel	w0, \1, \2, ne
**	ret
*/
int __GIMPLE (ssa)
clz5 (unsigned int x)
{
  int _1;

  __BB(2):
  _1 = .CLZ (x_2(D), 5);
  return _1;
}

/* The same for DImode.  */

/*
** clz64:
**	clz	x([0-9]+), x0
**	cmp	x0, 0
**	mov	(w[0-9]+), 7
**	csel	w0, w\1, \2, ne
**	ret
*/
int __GIMPLE (ssa)
clz64 (unsigned long long x)
{
  int _1;

  __BB(2):
  _1 = .CLZ (x_2(D), 7);
  return _1;
}
