/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
** (
**	ldr	d([0-9]+), \[x1\]
**	ldr	q([0-9]+), \[x0\]
**	saddw	v([0-9]+)\.4s, v\2\.4s, v\1\.4h
**	str	q\3, \[x0\]
** |
**	ldr	q([0-9]+), \[x0\]
**	ldr	d([0-9]+), \[x1\]
**	saddw	v([0-9]+)\.4s, v\4\.4s, v\5\.4h
**	str	q\6, \[x0\]
** )
**	ret
*/
void
foo (int *x, short *y)
{
  x[0] += y[0];
  x[1] += y[1];
  x[2] += y[2];
  x[3] += y[3];
}
