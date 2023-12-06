/* { dg-options "-O2 -fstack-clash-protection -fomit-frame-pointer --param stack-clash-protection-guard-size=12 -fsanitize=shadow-call-stack -ffixed-x18 -fno-stack-protector" } */
/* { dg-final { check-function-bodies "**" "" } } */

void f(int, ...);
void g();

/*
** test1:
**	...
**	str	x30, \[sp\]
**	sub	sp, sp, #4064
**	str	xzr, \[sp, #?1024\]
**	cbnz	w0, .*
**	bl	g
**	...
**	str	x26, \[sp, #?4128\]
**	...
*/
int test1(int z) {
  __uint128_t x = 0;
  int y[0x400];
  if (z)
    {
      asm volatile ("" :::
		    "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26");
      f(0, 0, 0, 0, 0, 0, 0, &y,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x);
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
      asm volatile ("" :::
		    "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26");
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

/*
** test3:
**	...
**	str	x30, \[sp\]
**	sub	sp, sp, #1024
**	cbnz	w0, .*
**	bl	g
**	...
*/
int test3(int z) {
  __uint128_t x = 0;
  int y[0x400];
  if (z)
    {
      asm volatile ("" :::
		    "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26");
      f(0, 0, 0, 0, 0, 0, 0, &y,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x);
    }
  g();
  return 1;
}
