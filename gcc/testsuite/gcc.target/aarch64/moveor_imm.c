/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** f1:
**	...
**	mov	x0, -6148914691236517206
**	eor	x0, x0, -9223372036854775807
**	...
*/

long f1 (void)
{
  return 0x2aaaaaaaaaaaaaab;
}

/*
** f2:
**	...
**	mov	x0, -1085102592571150096
**	eor	x0, x0, -2305843009213693951
**	...
*/

long f2 (void)
{
  return 0x10f0f0f0f0f0f0f1;
}

/*
** f3:
**	...
**	mov	x0, -3689348814741910324
**	eor	x0, x0, -4611686018427387903
**	...
*/

long f3 (void)
{
  return 0xccccccccccccccd;
}

/*
** f4:
**	...
**	mov	x0, -7378697629483820647
**	eor	x0, x0, -9223372036854775807
**	...
*/

long f4 (void)
{
  return 0x1999999999999998;
}

/*
** f5:
**	...
**	mov	x0, 3689348814741910323
**	eor	x0, x0, 864691128656461824
**	...
*/

long f5 (void)
{
  return 0x3f3333333f333333;
}
