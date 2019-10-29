/* { dg-do compile } */
/* { dg-options "-O -fshrink-wrap -fstack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** test_1:
**	str	x24, \[sp, -32\]!
**	cntb	x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x13
**	str	p4, \[sp\]
**	cbz	w0, [^\n]*
**	...
**	ptrue	p0\.b, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldr	x24, \[sp\], 32
**	ret
*/
svbool_t
test_1 (int n)
{
  asm volatile ("" ::: "x24");
  if (n)
    {
      volatile int x = 1;
      asm volatile ("" ::: "p4");
    }
  return svptrue_b8 ();
}

/*
** test_2:
**	str	x24, \[sp, -32\]!
**	cntb	x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x13
**	str	p4, \[sp\]
**	cbz	w0, [^\n]*
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	...
**	ptrue	p0\.b, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldr	x24, \[sp\], 32
**	ret
*/
svbool_t
test_2 (int n)
{
  asm volatile ("" ::: "x24");
  if (n)
    {
      volatile int x = 1;
      asm volatile ("" ::: "p4", "p5", "p6");
    }
  return svptrue_b8 ();
}
