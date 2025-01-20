/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* PR rtl-optmization/111949 */

/*
**func1:
**	bic	w([0-9]+), w0, w1
**	and	w0, w\1, 1
**      ret
*/

unsigned func1(unsigned a, bool b)
{
        int c = a & b;
        return (c ^ a)&1;
}

/*
**func2:
**	bic	w([0-9]+), w1, w0
**	and	w0, w\1, 255
**      ret
*/
unsigned func2(bool a, bool b)
{
  return ~a & b;
}

/*
**func3:
**	bic	w([0-9]+), w1, w0
**	and	w0, w\1, 1
**      ret
*/
bool func3(bool a, unsigned char b)
{
  return !a & b;
}
