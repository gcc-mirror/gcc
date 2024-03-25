// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -funwind-tables" }
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
** n_caller:	{ target lp64 }
**	stp	x30, (x19|x2[0-8]), \[sp, #?-96\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	stp	d8, d9, \[sp, #?32\]
**	stp	d10, d11, \[sp, #?48\]
**	stp	d12, d13, \[sp, #?64\]
**	stp	d14, d15, \[sp, #?80\]
**	mov	\1, x0
**	bl	ns_callee
**	smstart	sm
**	bl	s_callee
**	smstop	sm
**	bl	sc_callee
**	ldr	(x[0-9]+), \[\1\]
**	blr	\2
**	ldr	(x[0-9]+), \[\1, #?8\]
**	smstart	sm
**	blr	\3
**	smstop	sm
**	ldr	(x[0-9]+), \[\1, #?16\]
**	blr	\4
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x30, \1, \[sp\], #?96
**	ret
*/
void
n_caller (struct callbacks *c)
{
  ns_callee ();
  s_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr ();
  c->sc_ptr ();
}

/*
** s_caller:	{ target lp64 }
**	stp	x30, (x19|x2[0-8]), \[sp, #?-96\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	stp	d8, d9, \[sp, #?32\]
**	stp	d10, d11, \[sp, #?48\]
**	stp	d12, d13, \[sp, #?64\]
**	stp	d14, d15, \[sp, #?80\]
**	mov	\1, x0
**	smstop	sm
**	bl	ns_callee
**	smstart	sm
**	bl	s_callee
**	bl	sc_callee
**	ldr	(x[0-9]+), \[\1\]
**	smstop	sm
**	blr	\2
**	smstart	sm
**	ldr	(x[0-9]+), \[\1, #?8\]
**	blr	\3
**	ldr	(x[0-9]+), \[\1, #?16\]
**	blr	\4
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x30, \1, \[sp\], #?96
**	ret
*/
void
s_caller (struct callbacks *c) [[arm::streaming]]
{
  ns_callee ();
  s_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr ();
  c->sc_ptr ();
}

/*
** sc_caller_sme:
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
**	tbz	x16, 0, .*
**	smstop	sm
**	bl	ns_callee
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	smstart	sm
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, .*
**	smstart	sm
**	bl	s_callee
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, .*
**	smstop	sm
**	bl	sc_callee
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
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
**	stp	x29, x30, \[sp, #?-96\]!
**	mov	x29, sp
**	cntd	x16
**	str	x16, \[sp, #?24\]
**	stp	d8, d9, \[sp, #?32\]
**	stp	d10, d11, \[sp, #?48\]
**	stp	d12, d13, \[sp, #?64\]
**	stp	d14, d15, \[sp, #?80\]
**	bl	__arm_sme_state
**	str	x0, \[x29, #?16\]
**	...
**	bl	sc_callee
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
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
**	sub	sp, sp, #112
**	stp	x29, x30, \[sp, #?16\]
**	add	x29, sp, #?16
**	...
**	stp	d8, d9, \[sp, #?48\]
**	...
**	bl	__arm_sme_state
**	str	x0, \[x29, #?16\]
**	...
**	bl	ns_callee_stack
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	smstart	sm
**	...
*/
void
sc_caller_stack () [[arm::streaming_compatible]]
{
  ns_callee_stack (0, 0, 0, 0, 0, 0, 0, 0, 0);
}

/* { dg-final { scan-assembler {n_caller:(?:(?!ret).)*\.cfi_offset 46, -80\n} } } */
/* { dg-final { scan-assembler {s_caller:(?:(?!ret).)*\.cfi_offset 46, -80\n} } } */
/* { dg-final { scan-assembler {sc_caller_sme:(?:(?!ret).)*\.cfi_offset 46, -72\n} } } */
/* { dg-final { scan-assembler {sc_caller:(?:(?!ret).)*\.cfi_offset 46, -72\n} } } */
