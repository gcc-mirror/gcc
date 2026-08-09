/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* A 64-bit comparison against zero whose result is negated has to stay in
   the general registers.  Before, only the vector alternative accepted the
   zero, so the value was moved to and from a vector register.  */

/*
** f1:
**	cmp	x0, 0
**	csetm	x0, eq
**	ret
*/
long
f1 (long x)
{
  return -(long) (x == 0);
}

/*
** f2:
**	cmp	x0, 0
**	csetm	x0, ne
**	ret
*/
long
f2 (long x)
{
  return -(long) (x != 0);
}

/*
** f3:
**	cmp	x0, 0
**	csetm	x0, le
**	ret
*/
long
f3 (long x)
{
  return -(long) (x <= 0);
}

/*
** f4:
**	cmp	x0, x1
**	csetm	x0, eq
**	ret
*/
long
f4 (long x, long y)
{
  return -(long) (x == y);
}

/* { dg-final { scan-assembler-not "\\tfmov\\t" } } */
/* { dg-final { scan-assembler-not "\\tcmeq\\t" } } */
