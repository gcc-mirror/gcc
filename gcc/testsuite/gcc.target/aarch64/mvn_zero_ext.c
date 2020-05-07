/* { dg-do compile } */
/* { dg-options "-O2" } */

/*
** foo:
**	mvn	w0, w0
**	ret
*/
unsigned long long
foo (unsigned a)
{
  return ~a;
}

/* { dg-final { check-function-bodies "**" "" "" } } */
