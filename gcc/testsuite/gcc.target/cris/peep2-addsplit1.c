/* Check that "opsplit1" with PLUS does its job.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-leading-underscore" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int addsi (int x)
{
  return x + 64;
}

char addqi (char x)
{
  return x + 126;
}

short addhi (short x)
{
  return x - 64;
}

unsigned short addhi2 (short x)
{
  return x - 126;
}

/*
** addsi:
**	addq 63,.r10
**	ret
**	addq 1,.r10
*/

/*
** addqi:
**	addq 63,.r10
**	ret
**	addq 63,.r10
*/

/*
** addhi:
**	subq 63,.r10
**	ret
**	subq 1,.r10
*/

/*
** addhi2:
**	subq 63,.r10
**	ret
**	subq 63,.r10
*/
