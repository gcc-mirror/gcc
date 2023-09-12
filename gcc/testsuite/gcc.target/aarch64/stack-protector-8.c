/* { dg-options " -O -fstack-protector-strong -mstack-protector-guard=sysreg -mstack-protector-guard-reg=tpidr2_el0 -mstack-protector-guard-offset=16" } */
/* { dg-final { check-function-bodies "**" "" } } */

void g(void *);
__SVBool_t *h(void *);

/*
** test1:
**	sub	sp, sp, #288
**	stp	x29, x30, \[sp, #?272\]
**	add	x29, sp, #?272
**	mrs	(x[0-9]+), tpidr2_el0
**	ldr	(x[0-9]+), \[\1, #?16\]
**	str	\2, \[sp, #?264\]
**	mov	\2, #?0
**	add	x0, sp, #?8
**	bl	g
**	...
**	mrs	.*
**	...
**	bne	.*
**	...
**	ldp	x29, x30, \[sp, #?272\]
**	add	sp, sp, #?288
**	ret
**	bl	__stack_chk_fail
*/
int test1() {
  int y[0x40];
  g(y);
  return 1;
}

/*
** test2:
**	stp	x29, x30, \[sp, #?-16\]!
**	mov	x29, sp
**	sub	sp, sp, #1040
**	mrs	(x[0-9]+), tpidr2_el0
**	ldr	(x[0-9]+), \[\1, #?16\]
**	str	\2, \[sp, #?1032\]
**	mov	\2, #?0
**	add	x0, sp, #?8
**	bl	g
**	...
**	mrs	.*
**	...
**	bne	.*
**	...
**	add	sp, sp, #?1040
**	ldp	x29, x30, \[sp\], #?16
**	ret
**	bl	__stack_chk_fail
*/
int test2() {
  int y[0x100];
  g(y);
  return 1;
}

#pragma GCC target "+sve"

/*
** test3:
**	stp	x29, x30, \[sp, #?-16\]!
**	mov	x29, sp
**	addvl	sp, sp, #-18
**	...
**	str	p4, \[sp\]
**	...
**	sub	sp, sp, #272
**	mrs	(x[0-9]+), tpidr2_el0
**	ldr	(x[0-9]+), \[\1, #?16\]
**	str	\2, \[sp, #?264\]
**	mov	\2, #?0
**	add	x0, sp, #?8
**	bl	h
**	...
**	mrs	.*
**	...
**	bne	.*
**	...
**	add	sp, sp, #?272
**	...
**	ldr	p4, \[sp\]
**	...
**	addvl	sp, sp, #18
**	ldp	x29, x30, \[sp\], #?16
**	ret
**	bl	__stack_chk_fail
*/
__SVBool_t test3() {
  int y[0x40];
  return *h(y);
}
