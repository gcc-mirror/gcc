/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#define SIMD [[gnu::aarch64_vector_pcs]]

void callee_base (void);
void callee_simd (void) SIMD;
void callee_sve (__SVBool_t);

/*
** base_to_base:
**	b	callee_base
*/
void base_to_base (void) { return callee_base (); }

/*
** base_to_simd:
**	b	callee_simd
*/
void base_to_simd (void) { return callee_simd (); }

/*
** base_to_sve:
**	ldr	p0, \[x0\]
**	b	callee_sve
*/
void base_to_sve (__SVBool_t *ptr) { return callee_sve (*ptr); }

/*
** simd_to_base:
**	stp	x29, x30, [^\n]+
**	...
**	bl	callee_base
**	...
**	ldp	x29, x30, [^\n]+
**	ret
*/
void simd_to_base (void) SIMD { return callee_base (); }

/*
** simd_to_simd:
**	b	callee_simd
*/
void simd_to_simd (void) SIMD { return callee_simd (); }

/*
** simd_to_sve:
**	ldr	p0, \[x0\]
**	b	callee_sve
*/
void simd_to_sve (__SVBool_t *ptr) SIMD { return callee_sve (*ptr); }

/*
** sve_to_base:
**	stp	x29, x30, [^\n]+
**	...
**	bl	callee_base
**	...
**	ldp	x29, x30, [^\n]+
**	ret
*/
void sve_to_base (__SVBool_t pg) { return callee_base (); }

/*
** sve_to_simd:
**	stp	x29, x30, [^\n]+
**	...
**	bl	callee_simd
**	...
**	ldp	x29, x30, [^\n]+
**	ret
*/
void sve_to_simd (__SVBool_t pg) { return callee_simd (); }

/*
** sve_to_sve:
**	b	callee_sve
*/
void sve_to_sve (__SVBool_t pg) { return callee_sve (pg); }
