/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

/*
** foo1:
**	and	w0, w0, 1
**	str	w0, \[x1\]
**	ret
*/
void foo1(unsigned v, unsigned *p)
{
    *p = v & 1;
}

#pragma GCC target "+sve"

/*
** foo2:
**	and	w0, w0, 1
**	str	w0, \[x1\]
**	ret
*/
void foo2(unsigned v, unsigned *p)
{
    *p = v & 1;
}
