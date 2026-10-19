/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define N 1000
int a[N] = {0};
int b[N] = {0};

/*
** foo:
**	...
**	ldr	q[0-9]+, \[x[0-9]+, x[0-9]+\]
**	ldr	q[0-9]+, \[x[0-9]+, x[0-9]+\]
**	add	x[0-9]+, x[0-9]+, 16
**	cmgt	v[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s
**	umaxp	v[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s
**	fmov	x[0-9]+, d[0-9]+
**	cbn?z	x[0-9]+, \.L[0-9]+
**	...
**	mov	w0, [01]
**	ret
**	mov	w0, [01]
**	ret
*/
__attribute__ ((noipa, noinline))
int foo (void)
{
  for (int i = 0; i < N; i++)
    {
      if (a[i] > b[i])
	return 1;
    }
  return 0;
}
