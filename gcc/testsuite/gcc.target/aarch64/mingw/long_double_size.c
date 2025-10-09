/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/**
 * For aarch64-w64-mingw32 target, long double is 64 bits. Unlike in
 * aarch64-linux-gnu, where long double is 128 bits. The tests below validate
 * validate that. In aarch64-linux-gnu, the results would be the opposite.
 */

/*
** test:
**	fmov	d0, 4.0e\+0
**	ret
*/
long double
test ()
{
  long double i = 4;
  return i;
}

/* { dg-final { scan-assembler-not "ldr\tq\[0-9\]+, \[x\[0-9\]+\]*" } } */
