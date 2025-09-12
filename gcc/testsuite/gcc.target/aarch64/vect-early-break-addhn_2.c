/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-vect-details -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define TYPE long long
#define N 800

#pragma GCC target "+nosve"

TYPE a[N];

/*
** foo:
**	...
**	ldp	q[0-9]+, q[0-9]+, \[x[0-9]+\], 32
**	cmeq	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
**	cmeq	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
**	addhn	v[0-9]+.2s, v[0-9]+.2d, v[0-9]+.2d
**	fmov	x[0-9]+, d[0-9]+
**	...
*/

int foo ()
{
#pragma GCC unroll 4
  for (int i = 0; i < N; i++)
    if (a[i] == 124)
      return 1;

  return 0;
}

/* { dg-final { scan-tree-dump "VEC_TRUNC_ADD_HIGH" "vect" } } */
