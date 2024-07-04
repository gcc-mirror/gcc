// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -funwind-tables" }
// { dg-final { check-function-bodies "**" "" } }

__attribute__((aarch64_vector_pcs)) void ns_callee ();
__attribute__((aarch64_vector_pcs)) void s_callee () [[arm::streaming]];
__attribute__((aarch64_vector_pcs)) void sc_callee () [[arm::streaming_compatible]];

struct callbacks {
  __attribute__((aarch64_vector_pcs)) void (*ns_ptr) ();
  __attribute__((aarch64_vector_pcs)) void (*s_ptr) () [[arm::streaming]];
  __attribute__((aarch64_vector_pcs)) void (*sc_ptr) () [[arm::streaming_compatible]];
};

/*
** n_caller:	{ target lp64 }
**	stp	x30, (x19|x2[0-8]), \[sp, #?-288\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	stp	q8, q9, \[sp, #?32\]
**	stp	q10, q11, \[sp, #?64\]
**	stp	q12, q13, \[sp, #?96\]
**	stp	q14, q15, \[sp, #?128\]
**	stp	q16, q17, \[sp, #?160\]
**	stp	q18, q19, \[sp, #?192\]
**	stp	q20, q21, \[sp, #?224\]
**	stp	q22, q23, \[sp, #?256\]
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
**	ldp	q8, q9, \[sp, #?32\]
**	ldp	q10, q11, \[sp, #?64\]
**	ldp	q12, q13, \[sp, #?96\]
**	ldp	q14, q15, \[sp, #?128\]
**	ldp	q16, q17, \[sp, #?160\]
**	ldp	q18, q19, \[sp, #?192\]
**	ldp	q20, q21, \[sp, #?224\]
**	ldp	q22, q23, \[sp, #?256\]
**	ldp	x30, \1, \[sp\], #?288
**	ret
*/
void __attribute__((aarch64_vector_pcs))
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
**	stp	x30, (x19|x2[0-8]), \[sp, #?-288\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	stp	q8, q9, \[sp, #?32\]
**	stp	q10, q11, \[sp, #?64\]
**	stp	q12, q13, \[sp, #?96\]
**	stp	q14, q15, \[sp, #?128\]
**	stp	q16, q17, \[sp, #?160\]
**	stp	q18, q19, \[sp, #?192\]
**	stp	q20, q21, \[sp, #?224\]
**	stp	q22, q23, \[sp, #?256\]
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
**	ldp	q8, q9, \[sp, #?32\]
**	ldp	q10, q11, \[sp, #?64\]
**	ldp	q12, q13, \[sp, #?96\]
**	ldp	q14, q15, \[sp, #?128\]
**	ldp	q16, q17, \[sp, #?160\]
**	ldp	q18, q19, \[sp, #?192\]
**	ldp	q20, q21, \[sp, #?224\]
**	ldp	q22, q23, \[sp, #?256\]
**	ldp	x30, \1, \[sp\], #?288
**	ret
*/
void __attribute__((aarch64_vector_pcs))
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
** sc_caller:
**	stp	x29, x30, \[sp, #?-288\]!
**	mov	x29, sp
**	cntd	x16
**	str	x16, \[sp, #?24\]
**	stp	q8, q9, \[sp, #?32\]
**	stp	q10, q11, \[sp, #?64\]
**	stp	q12, q13, \[sp, #?96\]
**	stp	q14, q15, \[sp, #?128\]
**	stp	q16, q17, \[sp, #?160\]
**	stp	q18, q19, \[sp, #?192\]
**	stp	q20, q21, \[sp, #?224\]
**	stp	q22, q23, \[sp, #?256\]
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
**	ldp	q8, q9, \[sp, #?32\]
**	ldp	q10, q11, \[sp, #?64\]
**	ldp	q12, q13, \[sp, #?96\]
**	ldp	q14, q15, \[sp, #?128\]
**	ldp	q16, q17, \[sp, #?160\]
**	ldp	q18, q19, \[sp, #?192\]
**	ldp	q20, q21, \[sp, #?224\]
**	ldp	q22, q23, \[sp, #?256\]
**	ldp	x29, x30, \[sp\], #?288
**	ret
*/
void __attribute__((aarch64_vector_pcs))
sc_caller () [[arm::streaming_compatible]]
{
  ns_callee ();
  s_callee ();
  sc_callee ();
}

/* { dg-final { scan-assembler {n_caller:(?:(?!ret).)*\.cfi_offset 46, -272\n} } } */
/* { dg-final { scan-assembler {s_caller:(?:(?!ret).)*\.cfi_offset 46, -272\n} } } */
/* { dg-final { scan-assembler {sc_caller:(?:(?!ret).)*\.cfi_offset 46, -264\n} } } */
