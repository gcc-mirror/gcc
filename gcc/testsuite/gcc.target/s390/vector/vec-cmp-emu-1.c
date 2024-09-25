/* { dg-do compile } */
/* { dg-options "-O2 -mzarch -march=z13" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { check-function-bodies "**" "" "" } } */

typedef __attribute__ ((vector_size (16))) signed __int128 v1ti;
typedef __attribute__ ((vector_size (16))) unsigned __int128 uv1ti;

/*
** eq:
**	vceqg	(%v[0-9]+),%v[0-9]+,%v[0-9]+
**	vpdi	(%v[0-9]+),\1,\1,4
**	vn	%v24,(\1,\2|\2,\1)
**	br	%r14
*/

v1ti
eq (v1ti x, v1ti y)
{
  return x == y;
}

/*
** ueq:
**	vceqg	(%v[0-9]+),%v[0-9]+,%v[0-9]+
**	vpdi	(%v[0-9]+),\1,\1,4
**	vn	%v24,(\1,\2|\2,\1)
**	br	%r14
*/

uv1ti
ueq (uv1ti x, uv1ti y)
{
  return x == y;
}
