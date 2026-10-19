/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a+sve --param=aarch64-autovec-preference=sve-only -msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define N 1000
int a[N] = {0};
int b[N] = {0};

/*
** foo:
**	...
**	ptrue	p[0-9]+\.b, all
**	...
**	ld1w	z[0-9]+\.s, p[0-9]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]
**	ld1w	z[0-9]+\.s, p[0-9]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]
**	cmpgt	p[0-9]+\.s, p[0-9]+/z, z[0-9]+\.s, z[0-9]+\.s
**	b\.any	\.L[0-9]+
**	...
**	whilelo	p[0-9]+\.s, w[0-9]+, w[0-9]+
**	...
**	ldr	w[0-9]+, \[x[0-9]+, x[0-9]+, lsl 2\]
**	ldr	w[0-9]+, \[x[0-9]+, x[0-9]+, lsl 2\]
**	cmp	w[0-9]+, w[0-9]+
**	...
**	ret
**	...
*/
__attribute__ ((noipa, noinline))
int foo (void)
{
  for (int i = 0; i < N; i++)
    {
      if (a[i] > b[i])
	return i;
    }
  return 0;
}
