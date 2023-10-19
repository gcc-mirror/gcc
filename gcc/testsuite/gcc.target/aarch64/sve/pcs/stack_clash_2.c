/* { dg-do compile } */
/* { dg-options "-O -fshrink-wrap -fstack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

svbool_t take_stack_args (volatile void *, void *, int, int, int,
			  int, int, int, int);

/*
** test_1:
**	cntb	x12
**	add	x12, x12, #?16
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	add	sp, sp, #?16
**	ret
*/
svbool_t
test_1 (void)
{
  volatile int x = 1;
  asm volatile ("" ::: "p4");
  return svptrue_b8 ();
}

/*
** test_2:
**	stp	x24, x25, \[sp, -48\]!
**	str	x26, \[sp, 16\]
**	cntb	x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x13
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldr	x26, \[sp, 16\]
**	ldp	x24, x25, \[sp\], 48
**	ret
*/
svbool_t
test_2 (void)
{
  volatile int x = 1;
  asm volatile ("" ::: "p4", "x24", "x25", "x26");
  return svptrue_b8 ();
}

/*
** test_3:
**	cntb	x12
**	mov	x13, #?4128
**	add	x12, x12, x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x24, x25, \[x11\]
**	str	x26, \[x11, 16\]
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x24, x25, \[sp\]
**	ldr	x26, \[sp, 16\]
**	mov	x12, #?4128
**	add	sp, sp, x12
**	ret
*/
svbool_t
test_3 (void)
{
  volatile int x[1024];
  asm volatile ("" :: "r" (x) : "p4", "x24", "x25", "x26");
  return svptrue_b8 ();
}

/*
** test_4:
**	cntb	x12, all, mul #2
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.h, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #2
**	ret
*/
svbool_t
test_4 (void)
{
  volatile svint32_t b;
  b = svdup_s32 (1);
  asm volatile ("" ::: "p4");
  return svptrue_b16 ();
}

/*
** test_5:
**	cntb	x12, all, mul #2
**	add	x12, x12, #?32
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x24, x25, \[x11\]
**	str	x26, \[x11, 16\]
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.h, all
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x24, x25, \[sp\]
**	ldr	x26, \[sp, 16\]
**	addvl	sp, sp, #1
**	add	sp, sp, #?32
**	ret
*/
svbool_t
test_5 (void)
{
  volatile svint32_t b;
  b = svdup_s32 (1);
  asm volatile ("" ::: "p4", "x24", "x25", "x26");
  return svptrue_b16 ();
}

/*
** test_6:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	cntb	x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x13
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
svbool_t
test_6 (void)
{
  take_stack_args (0, 0, 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4");
  return svptrue_b8 ();
}

/*
** test_7:
**	cntb	x12
**	mov	x13, #?4112
**	add	x12, x12, x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x29, x30, \[x11\]
**	addvl	x29, sp, #1
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\]
**	mov	x12, #?4112
**	add	sp, sp, x12
**	ret
*/
svbool_t
test_7 (void)
{
  volatile int x[1024];
  take_stack_args (x, 0, 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4");
  return svptrue_b8 ();
}

/*
** test_8:
**	cntb	x12
**	mov	x13, #?4144
**	add	x12, x12, x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x29, x30, \[x11\]
**	addvl	x29, sp, #1
**	stp	x24, x25, \[x29, 16\]
**	str	x26, \[x29, 32\]
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\]
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	mov	x12, #?4144
**	add	sp, sp, x12
**	ret
*/
svbool_t
test_8 (void)
{
  volatile int x[1024];
  take_stack_args (x, 0, 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4", "x24", "x25", "x26");
  return svptrue_b8 ();
}

/*
** test_9:
**	cntb	x12
**	mov	x13, #?4112
**	add	x12, x12, x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x29, x30, \[x11\]
**	addvl	x29, sp, #1
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	addvl	sp, x29, #-1
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\]
**	mov	x12, #?4112
**	add	sp, sp, x12
**	ret
*/
svbool_t
test_9 (int n)
{
  volatile int x[1024];
  take_stack_args (x, __builtin_alloca (n), 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4");
  return svptrue_b8 ();
}

/*
** test_10:
**	cntb	x12
**	mov	x13, #?4144
**	add	x12, x12, x13
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x29, x30, \[x11\]
**	addvl	x29, sp, #1
**	stp	x24, x25, \[x29, 16\]
**	str	x26, \[x29, 32\]
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	addvl	sp, x29, #-1
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\]
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	mov	x12, #?4144
**	add	sp, sp, x12
**	ret
*/
svbool_t
test_10 (int n)
{
  volatile int x[1024];
  take_stack_args (x, __builtin_alloca (n), 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4", "x24", "x25", "x26");
  return svptrue_b8 ();
}

/*
** test_11:
**	cntb	x12
**	add	x12, x12, #?3008
**	add	x12, x12, #?126976
**	mov	x11, sp
**	...
**	sub	sp, sp, x12
**	addvl	x11, sp, #1
**	stp	x29, x30, \[x11\]
**	addvl	x29, sp, #1
**	stp	x24, x25, \[x29, 16\]
**	str	x26, \[x29, 32\]
**	str	p4, \[sp\]
**	sub	sp, sp, #?16
**	...
**	ptrue	p0\.b, all
**	addvl	sp, x29, #-1
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ldp	x29, x30, \[sp\]
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	add	sp, sp, #?3008
**	add	sp, sp, #?126976
**	ret
*/
svbool_t
test_11 (int n)
{
  volatile int x[0x7ee4];
  take_stack_args (x, __builtin_alloca (n), 1, 2, 3, 4, 5, 6, 7);
  asm volatile ("" ::: "p4", "x24", "x25", "x26");
  return svptrue_b8 ();
}
