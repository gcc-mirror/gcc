/* { dg-do compile } */
/* { dg-options "-O -fshrink-wrap -fstack-clash-protection -msve-vector-bits=1024 -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

svbool_t take_stack_args (volatile void *, void *, int, int, int,
			  int, int, int, int);

/*
** test_1:
**	sub	sp, sp, #144
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, vl128
**	ldr	p4, \[sp\]
**	add	sp, sp, #?144
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
**	sub	sp, sp, #176
**	stp	x24, x25, \[sp, 128\]
**	str	x26, \[sp, 144\]
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, vl128
**	ldr	p4, \[sp\]
**	ldp	x24, x25, \[sp, 128\]
**	ldr	x26, \[sp, 144\]
**	add	sp, sp, #?176
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
**	mov	x12, #?4256
**	sub	sp, sp, x12
**	stp	x24, x25, \[sp, 128\]
**	str	x26, \[sp, 144\]
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, vl128
**	ldr	p4, \[sp\]
**	ldp	x24, x25, \[sp, 128\]
**	ldr	x26, \[sp, 144\]
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
**	sub	sp, sp, #256
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.h, vl64
**	ldr	p4, \[sp\]
**	add	sp, sp, #?256
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
**	sub	sp, sp, #288
**	stp	x24, x25, \[sp, 128\]
**	str	x26, \[sp, 144\]
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.h, vl64
**	ldr	p4, \[sp\]
**	ldp	x24, x25, \[sp, 128\]
**	ldr	x26, \[sp, 144\]
**	add	sp, sp, #?288
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
**	sub	sp, sp, #128
**	str	p4, \[sp\]
**	...
**	ptrue	p0\.b, vl128
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
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
**	mov	x12, #?4240
**	sub	sp, sp, x12
**	stp	x29, x30, \[sp, 128\]
**	add	x29, sp, #?128
**	str	p4, \[sp\]
**	sub	sp, sp, #16
**	...
**	ptrue	p0\.b, vl128
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
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
**	mov	x12, #?4272
**	sub	sp, sp, x12
**	stp	x29, x30, \[sp, 128\]
**	add	x29, sp, #?128
**	stp	x24, x25, \[sp, 144\]
**	str	x26, \[sp, 160\]
**	str	p4, \[sp\]
**	sub	sp, sp, #16
**	...
**	ptrue	p0\.b, vl128
**	add	sp, sp, #?16
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	ldp	x29, x30, \[sp\]
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
**	mov	x12, #?4240
**	sub	sp, sp, x12
**	stp	x29, x30, \[sp, 128\]
**	add	x29, sp, #?128
**	str	p4, \[sp\]
**	sub	sp, sp, #16
**	...
**	ptrue	p0\.b, vl128
**	sub	sp, x29, #128
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
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
**	mov	x12, #?4272
**	sub	sp, sp, x12
**	stp	x29, x30, \[sp, 128\]
**	add	x29, sp, #?128
**	stp	x24, x25, \[sp, 144\]
**	str	x26, \[sp, 160\]
**	str	p4, \[sp\]
**	sub	sp, sp, #16
**	...
**	ptrue	p0\.b, vl128
**	sub	sp, x29, #128
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	ldp	x29, x30, \[sp\]
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
**	sub	sp, sp, #65536
**	str	xzr, \[sp, 1024\]
**	mov	x12, #?64576
**	sub	sp, sp, x12
**	str	xzr, \[sp, 1024\]
**	stp	x29, x30, \[sp, 128\]
**	add	x29, sp, #?128
**	stp	x24, x25, \[sp, 144\]
**	str	x26, \[sp, 160\]
**	str	p4, \[sp\]
**	sub	sp, sp, #16
**	...
**	ptrue	p0\.b, vl128
**	sub	sp, x29, #128
**	ldr	p4, \[sp\]
**	add	sp, sp, #?128
**	ldp	x24, x25, \[sp, 16\]
**	ldr	x26, \[sp, 32\]
**	ldp	x29, x30, \[sp\]
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
