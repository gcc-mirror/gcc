/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */


/*
** f1:
**	mov	x0, 20014547599360
**	add	x0, x0, 5664768
**	ret
*/

long f1 (void)
{
  return 0x123400567000;
}

/*
** f2:
**	mov	x0, 20014547599360
**	sub	x0, x0, #11112448
**	ret
*/

long f2 (void)
{
  return 0x1233ff567000;
}

/*
** f3:
**	mov	x0, 1311673391471656959
**	add	x0, x0, 5668864
**	ret
*/

long f3 (void)
{
  return 0x1234000000567fff;
}

/*
** f4:
**	mov	x0, 1312236341425078271
**	sub	x0, x0, #11108352
**	ret
*/

long f4 (void)
{
  return 0x1235ffffff567fff;
}

/*
** f5:
**	mov	x0, 8102099357864587376
**	add	x0, x0, 10600448
**	ret
*/

long f5 (void)
{
  return 0x7070707071123070;
}

/*
** f6:
**	mov	x0, 8102099357864587376
**	sub	x0, x0, #8273920
**	ret
*/

long f6 (void)
{
  return 0x707070706ff23070;
}
