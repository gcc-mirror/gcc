/* PR target/127538: Verify that unsigned conditional subtraction is
   emitted as SUBS followed by CSEL, rather than as separate CMP/CSEL/SUB
   instructions.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** conditional_subtract:
**	subs\t(x[0-9]+), x0, x1
**	csel\tx0, \1, x0, cs
**	ret
*/
unsigned long long
conditional_subtract (unsigned long long x, unsigned long long q)
{
  return x - (x >= q ? q : 0);
}

/*
** conditional_subtract_si:
**	subs\t(w[0-9]+), w0, w1
**	csel\tw0, \1, w0, cs
**	ret
*/
unsigned int
conditional_subtract_si (unsigned int x, unsigned int q)
{
  return x - (x >= q ? q : 0);
}

