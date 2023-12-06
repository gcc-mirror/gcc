/* { dg-options "-O2 -fstack-clash-protection -fomit-frame-pointer --param stack-clash-protection-guard-size=12 -fno-stack-protector" } */
/* { dg-final { check-function-bodies "**" "" } } */

void f(int, ...);
void g();

/*
** test1:
**	...
**	str	x30, \[sp\]
**	sub	sp, sp, #1024
**	cbnz	w0, .*
**	bl	g
**	...
*/
int test1(int z) {
  __uint128_t x = 0;
  int y[0x400];
  if (z)
    {
      f(0, 0, 0, 0, 0, 0, 0, &y,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x);
    }
  g();
  return 1;
}

/*
** test2:
**	...
**	str	x30, \[sp\]
**	sub	sp, sp, #1040
**	str	xzr, \[sp, #?1024\]
**	cbnz	w0, .*
**	bl	g
**	...
*/
int test2(int z) {
  __uint128_t x = 0;
  int y[0x400];
  if (z)
    {
      f(0, 0, 0, 0, 0, 0, 0, &y,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x);
    }
  g();
  return 1;
}
