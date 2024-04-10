// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -funwind-tables -mtrack-speculation" }
// { dg-final { check-function-bodies "**" "" } }

void ns_callee ();
 void s_callee () [[arm::streaming]];
 void sc_callee () [[arm::streaming_compatible]];

void ns_callee_stack (int, int, int, int, int, int, int, int, int);

struct callbacks {
  void (*ns_ptr) ();
  void (*s_ptr) () [[arm::streaming]];
  void (*sc_ptr) () [[arm::streaming_compatible]];
};

/*
** sc_caller_sme:
**	cmp	sp, #?0
**	csetm	x15, ne
**	stp	x29, x30, \[sp, #?-96\]!
**	mov	x29, sp
**	cntd	x16
**	str	x16, \[sp, #?24\]
**	stp	d8, d9, \[sp, #?32\]
**	stp	d10, d11, \[sp, #?48\]
**	stp	d12, d13, \[sp, #?64\]
**	stp	d14, d15, \[sp, #?80\]
**	mrs	x16, svcr
**	str	x16, \[x29, #?16\]
**	ldr	x16, \[x29, #?16\]
**	tst	x16, #?1
**	beq	[^\n]*
**	csel	x15, x15, xzr, ne
**	smstop	sm
**	b	[^\n]*
**	csel	x15, x15, xzr, eq
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	ns_callee
**	cmp	sp, #?0
**	csetm	x15, ne
**	ldr	x16, \[x29, #?16\]
**	tst	x16, #?1
**	beq	[^\n]*
**	csel	x15, x15, xzr, ne
**	smstart	sm
**	b	[^\n]*
**	csel	x15, x15, xzr, eq
**	ldr	x16, \[x29, #?16\]
**	tst	x16, #?1
**	bne	[^\n]*
**	csel	x15, x15, xzr, eq
**	smstart	sm
**	b	[^\n]*
**	csel	x15, x15, xzr, ne
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	s_callee
**	cmp	sp, #?0
**	csetm	x15, ne
**	ldr	x16, \[x29, #?16\]
**	tst	x16, #?1
**	bne	[^\n]*
**	csel	x15, x15, xzr, eq
**	smstop	sm
**	b	[^\n]*
**	csel	x15, x15, xzr, ne
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	sc_callee
**	cmp	sp, #?0
**	csetm	x15, ne
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	ret
*/
void
sc_caller_sme () [[arm::streaming_compatible]]
{
  ns_callee ();
  s_callee ();
  sc_callee ();
}

#pragma GCC target "+nosme"

/*
** sc_caller:
**	cmp	sp, #?0
**	csetm	x15, ne
**	stp	x29, x30, \[sp, #?-96\]!
**	mov	x29, sp
**	cntd	x16
**	str	x16, \[sp, #?24\]
**	stp	d8, d9, \[sp, #?32\]
**	stp	d10, d11, \[sp, #?48\]
**	stp	d12, d13, \[sp, #?64\]
**	stp	d14, d15, \[sp, #?80\]
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	__arm_sme_state
**	cmp	sp, #?0
**	csetm	x15, ne
**	str	x0, \[x29, #?16\]
**	...
**	bl	sc_callee
**	cmp	sp, #?0
**	csetm	x15, ne
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	ret
*/
void
sc_caller () [[arm::streaming_compatible]]
{
  ns_callee ();
  sc_callee ();
}

/*
** sc_caller_x0:
**	...
**	mov	x10, x0
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	__arm_sme_state
**	...
**	str	wzr, \[x10\]
**	...
*/
void
sc_caller_x0 (int *ptr) [[arm::streaming_compatible]]
{
  *ptr = 0;
  ns_callee ();
  sc_callee ();
}

/*
** sc_caller_x1:
**	...
**	mov	x10, x0
**	mov	x11, x1
**	mov	x14, sp
**	and	x14, x14, x15
**	mov	sp, x14
**	bl	__arm_sme_state
**	...
**	str	w11, \[x10\]
**	...
*/
void
sc_caller_x1 (int *ptr, int a) [[arm::streaming_compatible]]
{
  *ptr = a;
  ns_callee ();
  sc_callee ();
}

/*
** sc_caller_stack:
**	cmp	sp, #?0
**	csetm	x15, ne
**	sub	sp, sp, #112
**	stp	x29, x30, \[sp, #?16\]
**	add	x29, sp, #?16
**	...
**	stp	d8, d9, \[sp, #?48\]
**	...
**	bl	__arm_sme_state
**	cmp	sp, #?0
**	csetm	x15, ne
**	str	x0, \[x29, #?16\]
**	...
**	bl	ns_callee_stack
**	cmp	sp, #?0
**	csetm	x15, ne
**	ldr	x16, \[x29, #?16\]
**	tst	x16, #?1
**	beq	[^\n]*
**	csel	x15, x15, xzr, ne
**	smstart	sm
**	...
*/
void
sc_caller_stack () [[arm::streaming_compatible]]
{
  ns_callee_stack (0, 0, 0, 0, 0, 0, 0, 0, 0);
}

/* { dg-final { scan-assembler {sc_caller_sme:(?:(?!ret).)*\.cfi_offset 46, -72\n} } } */
/* { dg-final { scan-assembler {sc_caller:(?:(?!ret).)*\.cfi_offset 46, -72\n} } } */
