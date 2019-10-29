/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

int sve_callee (svint8_t);

/*
** standard_caller:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.b, #1
**	bl	sve_callee
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
int standard_caller (void) { return sve_callee (svdup_s8 (1)) + 1; }

/*
** vpcs_caller:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.b, #1
**	bl	sve_callee
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
__attribute__((aarch64_vector_pcs))
int vpcs_caller (void) { return sve_callee (svdup_s8 (1)) + 1; }

/*
** sve_caller:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.b, #1
**	bl	sve_callee
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
int sve_caller (svbool_t p0) { return sve_callee (svdup_s8 (1)) + 1; }

/*
** standard_caller_ptr:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.h, #1
**	blr	x0
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
int
standard_caller_ptr (int (*fn) (__SVInt16_t))
{
  return fn (svdup_s16 (1)) + 1;
}

/*
** vpcs_caller_ptr:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.h, #1
**	blr	x0
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
int __attribute__((aarch64_vector_pcs))
vpcs_caller_ptr (int (*fn) (__SVInt16_t))
{
  return fn (svdup_s16 (1)) + 1;
}

/*
** sve_caller_ptr:
**	stp	x29, x30, \[sp, -16\]!
**	mov	x29, sp
**	mov	z0\.h, #1
**	blr	x0
**	add	w0, w0, #?1
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
int
sve_caller_ptr (svbool_t pg, int (*fn) (svint16_t))
{
  return fn (svdup_s16 (1)) + 1;
}
