// { dg-options "-O -fomit-frame-pointer -fno-stack-clash-protection" }
// { dg-final { check-function-bodies "**" "" } }

void consume_za () [[arm::streaming, arm::inout("za")]];

/*
** n_ls:
**	sub	sp, sp, #?80
**	cntd	x16
**	str	x16, \[sp\]
**	stp	d8, d9, \[sp, #?16\]
**	stp	d10, d11, \[sp, #?32\]
**	stp	d12, d13, \[sp, #?48\]
**	stp	d14, d15, \[sp, #?64\]
**	smstart	sm
**	smstop	sm
**	ldp	d8, d9, \[sp, #?16\]
**	ldp	d10, d11, \[sp, #?32\]
**	ldp	d12, d13, \[sp, #?48\]
**	ldp	d14, d15, \[sp, #?64\]
**	add	sp, sp, #?80
**	ret
*/
[[arm::locally_streaming]] void
n_ls ()
{
  asm ("");
}

/*
** s_ls:
**	ret
*/
[[arm::locally_streaming]] void
s_ls () [[arm::streaming]]
{
  asm ("");
}

/*
** sc_ls:
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
**	tbnz	x16, 0, [^\n]+
**	smstart	sm
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, [^\n]+
**	smstop	sm
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
**	ret
*/
[[arm::locally_streaming]] void
sc_ls () [[arm::streaming_compatible]]
{
  asm ("");
}

/*
** n_ls_new_za:
**	str	x30, \[sp, #?-80\]!
**	cntd	x16
**	str	x16, \[sp, #?8\]
**	stp	d8, d9, \[sp, #?16\]
**	stp	d10, d11, \[sp, #?32\]
**	stp	d12, d13, \[sp, #?48\]
**	stp	d14, d15, \[sp, #?64\]
**	smstart	sm
**	mrs	(x[0-9]+), tpidr2_el0
**	cbz	\1, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	zero	{ za }
**	smstart	za
**	bl	consume_za
**	smstop	za
**	smstop	sm
**	ldp	d8, d9, \[sp, #?16\]
**	ldp	d10, d11, \[sp, #?32\]
**	ldp	d12, d13, \[sp, #?48\]
**	ldp	d14, d15, \[sp, #?64\]
**	ldr	x30, \[sp\], #?80
**	ret
*/
[[arm::locally_streaming, arm::new("za")]] void
n_ls_new_za ()
{
  consume_za ();
  asm ("");
}

/*
** s_ls_new_za:
**	str	x30, \[sp, #?-16\]!
**	mrs	(x[0-9]+), tpidr2_el0
**	cbz	\1, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	zero	{ za }
**	smstart	za
**	bl	consume_za
**	smstop	za
**	ldr	x30, \[sp\], #?16
**	ret
*/
[[arm::locally_streaming, arm::new("za")]] void
s_ls_new_za () [[arm::streaming]]
{
  consume_za ();
  asm ("");
}

/*
** sc_ls_new_za:
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
**	tbnz	x16, 0, [^\n]+
**	smstart	sm
**	mrs	(x[0-9]+), tpidr2_el0
**	cbz	\1, [^\n]+
**	bl	__arm_tpidr2_save
**	msr	tpidr2_el0, xzr
**	zero	{ za }
**	smstart	za
**	bl	consume_za
**	smstop	za
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, [^\n]+
**	smstop	sm
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
**	ret
*/
[[arm::locally_streaming, arm::new("za")]] void
sc_ls_new_za () [[arm::streaming_compatible]]
{
  consume_za ();
  asm ("");
}

/*
** n_ls_shared_za:
**	str	x30, \[sp, #?-80\]!
**	cntd	x16
**	str	x16, \[sp, #?8\]
**	stp	d8, d9, \[sp, #?16\]
**	stp	d10, d11, \[sp, #?32\]
**	stp	d12, d13, \[sp, #?48\]
**	stp	d14, d15, \[sp, #?64\]
**	smstart	sm
**	bl	consume_za
**	smstop	sm
**	ldp	d8, d9, \[sp, #?16\]
**	ldp	d10, d11, \[sp, #?32\]
**	ldp	d12, d13, \[sp, #?48\]
**	ldp	d14, d15, \[sp, #?64\]
**	ldr	x30, \[sp\], #?80
**	ret
*/
[[arm::locally_streaming]] void
n_ls_shared_za () [[arm::inout("za")]]
{
  consume_za ();
  asm ("");
}

/*
** s_ls_shared_za:
**	str	x30, \[sp, #?-16\]!
**	bl	consume_za
**	ldr	x30, \[sp\], #?16
**	ret
*/
[[arm::locally_streaming]] void
s_ls_shared_za () [[arm::streaming, arm::inout("za")]]
{
  consume_za ();
  asm ("");
}

/*
** sc_ls_shared_za:
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
**	tbnz	x16, 0, [^\n]+
**	smstart	sm
**	bl	consume_za
**	ldr	x16, \[x29, #?16\]
**	tbnz	x16, 0, [^\n]+
**	smstop	sm
**	ldp	d8, d9, \[sp, #?32\]
**	ldp	d10, d11, \[sp, #?48\]
**	ldp	d12, d13, \[sp, #?64\]
**	ldp	d14, d15, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?96
**	ret
*/
[[arm::locally_streaming]] void
sc_ls_shared_za () [[arm::streaming_compatible, arm::inout("za")]]
{
  consume_za ();
  asm ("");
}

/*
** n_ls_vector_pcs:
**	sub	sp, sp, #?272
**	cntd	x16
**	str	x16, \[sp\]
**	stp	q8, q9, \[sp, #?16\]
**	stp	q10, q11, \[sp, #?48\]
**	stp	q12, q13, \[sp, #?80\]
**	stp	q14, q15, \[sp, #?112\]
**	stp	q16, q17, \[sp, #?144\]
**	stp	q18, q19, \[sp, #?176\]
**	stp	q20, q21, \[sp, #?208\]
**	stp	q22, q23, \[sp, #?240\]
**	smstart	sm
**	smstop	sm
**	ldp	q8, q9, \[sp, #?16\]
**	ldp	q10, q11, \[sp, #?48\]
**	ldp	q12, q13, \[sp, #?80\]
**	ldp	q14, q15, \[sp, #?112\]
**	ldp	q16, q17, \[sp, #?144\]
**	ldp	q18, q19, \[sp, #?176\]
**	ldp	q20, q21, \[sp, #?208\]
**	ldp	q22, q23, \[sp, #?240\]
**	add	sp, sp, #?272
**	ret
*/
[[arm::locally_streaming]] void __attribute__((aarch64_vector_pcs))
n_ls_vector_pcs ()
{
  asm ("");
}

/*
** n_ls_sve_pcs:	{ target aarch64_little_endian }
**	sub	sp, sp, #?16
**	cntd	x16
**	str	x16, \[sp\]
**	addsvl	sp, sp, #-18
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
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	smstop	sm
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
**	addsvl	sp, sp, #18
**	add	sp, sp, #?16
**	ret
*/
[[arm::locally_streaming]] void
n_ls_sve_pcs (__SVBool_t x)
{
  asm ("");
}

/*
** n_ls_v0:
**	addsvl	sp, sp, #-1
**	...
**	smstart	sm
**	add	x[0-9]+, [^\n]+
**	smstop	sm
**	...
**	addsvl	sp, sp, #1
**	...
*/
#define TEST(VN) __SVInt32_t VN; asm ("" :: "r" (&VN));
[[arm::locally_streaming]] void
n_ls_v0 ()
{
  TEST (v0);
}

/*
** n_ls_v32:
**	addsvl	sp, sp, #-32
**	...
**	smstart	sm
**	...
**	smstop	sm
**	...
**	rdsvl	(x[0-9]+), #1
**	lsl	(x[0-9]+), \1, #?5
**	add	sp, sp, \2
**	...
*/
[[arm::locally_streaming]] void
n_ls_v32 ()
{
  TEST (v0);
  TEST (v1);
  TEST (v2);
  TEST (v3);
  TEST (v4);
  TEST (v5);
  TEST (v6);
  TEST (v7);
  TEST (v8);
  TEST (v9);
  TEST (v10);
  TEST (v11);
  TEST (v12);
  TEST (v13);
  TEST (v14);
  TEST (v15);
  TEST (v16);
  TEST (v17);
  TEST (v18);
  TEST (v19);
  TEST (v20);
  TEST (v21);
  TEST (v22);
  TEST (v23);
  TEST (v24);
  TEST (v25);
  TEST (v26);
  TEST (v27);
  TEST (v28);
  TEST (v29);
  TEST (v30);
  TEST (v31);
}

/*
** n_ls_v33:
**	rdsvl	(x[0-9]+), #1
**	mov	(x[0-9]+), #?33
**	mul	(x[0-9]+), (?:\1, \2|\2, \1)
**	sub	sp, sp, \3
**	...
**	smstart	sm
**	...
**	smstop	sm
**	...
**	rdsvl	(x[0-9]+), #1
**	mov	(x[0-9]+), #?33
**	mul	(x[0-9]+), (?:\4, \5|\5, \4)
**	add	sp, sp, \6
**	...
*/
[[arm::locally_streaming]] void
n_ls_v33 ()
{
  TEST (v0);
  TEST (v1);
  TEST (v2);
  TEST (v3);
  TEST (v4);
  TEST (v5);
  TEST (v6);
  TEST (v7);
  TEST (v8);
  TEST (v9);
  TEST (v10);
  TEST (v11);
  TEST (v12);
  TEST (v13);
  TEST (v14);
  TEST (v15);
  TEST (v16);
  TEST (v17);
  TEST (v18);
  TEST (v19);
  TEST (v20);
  TEST (v21);
  TEST (v22);
  TEST (v23);
  TEST (v24);
  TEST (v25);
  TEST (v26);
  TEST (v27);
  TEST (v28);
  TEST (v29);
  TEST (v30);
  TEST (v31);
  TEST (v32);
}
