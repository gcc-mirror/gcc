// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -funwind-tables -fno-stack-clash-protection" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sve.h>

svbool_t ns_callee ();
 svbool_t s_callee () [[arm::streaming]];
 svbool_t sc_callee () [[arm::streaming_compatible]];

struct callbacks {
  svbool_t (*ns_ptr) ();
   svbool_t (*s_ptr) () [[arm::streaming]];
   svbool_t (*sc_ptr) () [[arm::streaming_compatible]];
};

/*
** n_caller:	{ target { lp64 && aarch64_little_endian } }
**	stp	x30, (x19|x2[0-8]), \[sp, #?-32\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	addvl	sp, sp, #-18
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	p12, \[sp, #8, mul vl\]
**	str	p13, \[sp, #9, mul vl\]
**	str	p14, \[sp, #10, mul vl\]
**	str	p15, \[sp, #11, mul vl\]
**	str	z8, \[sp, #2, mul vl\]
**	str	z9, \[sp, #3, mul vl\]
**	str	z10, \[sp, #4, mul vl\]
**	str	z11, \[sp, #5, mul vl\]
**	str	z12, \[sp, #6, mul vl\]
**	str	z13, \[sp, #7, mul vl\]
**	str	z14, \[sp, #8, mul vl\]
**	str	z15, \[sp, #9, mul vl\]
**	str	z16, \[sp, #10, mul vl\]
**	str	z17, \[sp, #11, mul vl\]
**	str	z18, \[sp, #12, mul vl\]
**	str	z19, \[sp, #13, mul vl\]
**	str	z20, \[sp, #14, mul vl\]
**	str	z21, \[sp, #15, mul vl\]
**	str	z22, \[sp, #16, mul vl\]
**	str	z23, \[sp, #17, mul vl\]
**	mov	\1, x0
**	bl	ns_callee
**	smstart	sm
**	bl	s_callee
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	bl	sc_callee
**	ldr	(x[0-9]+), \[\1\]
**	blr	\2
**	ldr	(x[0-9]+), \[\1, #?8\]
**	smstart	sm
**	blr	\3
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	ldr	(x[0-9]+), \[\1, #?16\]
**	blr	\4
**	ldr	z8, \[sp, #2, mul vl\]
**	ldr	z9, \[sp, #3, mul vl\]
**	ldr	z10, \[sp, #4, mul vl\]
**	ldr	z11, \[sp, #5, mul vl\]
**	ldr	z12, \[sp, #6, mul vl\]
**	ldr	z13, \[sp, #7, mul vl\]
**	ldr	z14, \[sp, #8, mul vl\]
**	ldr	z15, \[sp, #9, mul vl\]
**	ldr	z16, \[sp, #10, mul vl\]
**	ldr	z17, \[sp, #11, mul vl\]
**	ldr	z18, \[sp, #12, mul vl\]
**	ldr	z19, \[sp, #13, mul vl\]
**	ldr	z20, \[sp, #14, mul vl\]
**	ldr	z21, \[sp, #15, mul vl\]
**	ldr	z22, \[sp, #16, mul vl\]
**	ldr	z23, \[sp, #17, mul vl\]
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	ldr	p12, \[sp, #8, mul vl\]
**	ldr	p13, \[sp, #9, mul vl\]
**	ldr	p14, \[sp, #10, mul vl\]
**	ldr	p15, \[sp, #11, mul vl\]
**	addvl	sp, sp, #18
**	ldp	x30, \1, \[sp\], #?32
**	ret
*/
svbool_t
n_caller (struct callbacks *c)
{
  ns_callee ();
  s_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr ();
  return c->sc_ptr ();
}

/*
** s_caller:	{ target { lp64 && aarch64_little_endian } }
**	stp	x30, (x19|x2[0-8]), \[sp, #?-32\]!
**	cntd	x16
**	str	x16, \[sp, #?16\]
**	addvl	sp, sp, #-18
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	p12, \[sp, #8, mul vl\]
**	str	p13, \[sp, #9, mul vl\]
**	str	p14, \[sp, #10, mul vl\]
**	str	p15, \[sp, #11, mul vl\]
**	str	z8, \[sp, #2, mul vl\]
**	str	z9, \[sp, #3, mul vl\]
**	str	z10, \[sp, #4, mul vl\]
**	str	z11, \[sp, #5, mul vl\]
**	str	z12, \[sp, #6, mul vl\]
**	str	z13, \[sp, #7, mul vl\]
**	str	z14, \[sp, #8, mul vl\]
**	str	z15, \[sp, #9, mul vl\]
**	str	z16, \[sp, #10, mul vl\]
**	str	z17, \[sp, #11, mul vl\]
**	str	z18, \[sp, #12, mul vl\]
**	str	z19, \[sp, #13, mul vl\]
**	str	z20, \[sp, #14, mul vl\]
**	str	z21, \[sp, #15, mul vl\]
**	str	z22, \[sp, #16, mul vl\]
**	str	z23, \[sp, #17, mul vl\]
**	mov	\1, x0
**	smstop	sm
**	bl	ns_callee
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	bl	s_callee
**	bl	sc_callee
**	ldr	(x[0-9]+), \[\1\]
**	smstop	sm
**	blr	\2
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	ldr	(x[0-9]+), \[\1, #?8\]
**	blr	\3
**	ldr	(x[0-9]+), \[\1, #?16\]
**	blr	\4
**	ldr	z8, \[sp, #2, mul vl\]
**	ldr	z9, \[sp, #3, mul vl\]
**	ldr	z10, \[sp, #4, mul vl\]
**	ldr	z11, \[sp, #5, mul vl\]
**	ldr	z12, \[sp, #6, mul vl\]
**	ldr	z13, \[sp, #7, mul vl\]
**	ldr	z14, \[sp, #8, mul vl\]
**	ldr	z15, \[sp, #9, mul vl\]
**	ldr	z16, \[sp, #10, mul vl\]
**	ldr	z17, \[sp, #11, mul vl\]
**	ldr	z18, \[sp, #12, mul vl\]
**	ldr	z19, \[sp, #13, mul vl\]
**	ldr	z20, \[sp, #14, mul vl\]
**	ldr	z21, \[sp, #15, mul vl\]
**	ldr	z22, \[sp, #16, mul vl\]
**	ldr	z23, \[sp, #17, mul vl\]
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	ldr	p12, \[sp, #8, mul vl\]
**	ldr	p13, \[sp, #9, mul vl\]
**	ldr	p14, \[sp, #10, mul vl\]
**	ldr	p15, \[sp, #11, mul vl\]
**	addvl	sp, sp, #18
**	ldp	x30, \1, \[sp\], #?32
**	ret
*/
svbool_t
s_caller (struct callbacks *c) [[arm::streaming]]
{
  ns_callee ();
  s_callee ();
  sc_callee ();

  c->ns_ptr ();
  c->s_ptr ();
  return c->sc_ptr ();
}

/*
** sc_caller:	{ target aarch64_little_endian }
**	stp	x29, x30, \[sp, #?-32\]!
**	mov	x29, sp
**	cntd	x16
**	str	x16, \[sp, #?24\]
**	addvl	sp, sp, #-18
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	p12, \[sp, #8, mul vl\]
**	str	p13, \[sp, #9, mul vl\]
**	str	p14, \[sp, #10, mul vl\]
**	str	p15, \[sp, #11, mul vl\]
**	str	z8, \[sp, #2, mul vl\]
**	str	z9, \[sp, #3, mul vl\]
**	str	z10, \[sp, #4, mul vl\]
**	str	z11, \[sp, #5, mul vl\]
**	str	z12, \[sp, #6, mul vl\]
**	str	z13, \[sp, #7, mul vl\]
**	str	z14, \[sp, #8, mul vl\]
**	str	z15, \[sp, #9, mul vl\]
**	str	z16, \[sp, #10, mul vl\]
**	str	z17, \[sp, #11, mul vl\]
**	str	z18, \[sp, #12, mul vl\]
**	str	z19, \[sp, #13, mul vl\]
**	str	z20, \[sp, #14, mul vl\]
**	str	z21, \[sp, #15, mul vl\]
**	str	z22, \[sp, #16, mul vl\]
**	str	z23, \[sp, #17, mul vl\]
**	mrs	x16, svcr
**	str	x16, \[x29, #?16\]
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	smstop	sm
**	bl	ns_callee
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, .*
**	smstart	sm
**	bl	s_callee
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, .*
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	bl	sc_callee
**	ldr	z8, \[sp, #2, mul vl\]
**	ldr	z9, \[sp, #3, mul vl\]
**	ldr	z10, \[sp, #4, mul vl\]
**	ldr	z11, \[sp, #5, mul vl\]
**	ldr	z12, \[sp, #6, mul vl\]
**	ldr	z13, \[sp, #7, mul vl\]
**	ldr	z14, \[sp, #8, mul vl\]
**	ldr	z15, \[sp, #9, mul vl\]
**	ldr	z16, \[sp, #10, mul vl\]
**	ldr	z17, \[sp, #11, mul vl\]
**	ldr	z18, \[sp, #12, mul vl\]
**	ldr	z19, \[sp, #13, mul vl\]
**	ldr	z20, \[sp, #14, mul vl\]
**	ldr	z21, \[sp, #15, mul vl\]
**	ldr	z22, \[sp, #16, mul vl\]
**	ldr	z23, \[sp, #17, mul vl\]
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	ldr	p12, \[sp, #8, mul vl\]
**	ldr	p13, \[sp, #9, mul vl\]
**	ldr	p14, \[sp, #10, mul vl\]
**	ldr	p15, \[sp, #11, mul vl\]
**	addvl	sp, sp, #18
**	ldp	x29, x30, \[sp\], #?32
**	ret
*/
svbool_t
sc_caller () [[arm::streaming_compatible]]
{
  ns_callee ();
  s_callee ();
  return sc_callee ();
}

/* { dg-final { scan-assembler {n_caller:(?:(?!ret).)*\.cfi_offset 46, -16\n} } } */
/* { dg-final { scan-assembler {s_caller:(?:(?!ret).)*\.cfi_offset 46, -16\n} } } */
/* { dg-final { scan-assembler {sc_caller:(?:(?!ret).)*\.cfi_offset 46, -8\n} } } */
