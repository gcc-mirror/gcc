/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */
/* { dg-final { check-function-bodies "**" "" } } */

union u_t_0 {
    float f;
};
union u_t_1 {
    double d;
};

typedef void (__attribute__ ((cmse_nonsecure_call)) fn_t_0)(union u_t_0);
typedef void (__attribute__ ((cmse_nonsecure_call)) fn_t_1)(union u_t_1);

void fn_caller_0 (fn_t_0 *f_ptr) {
    union u_t_0 x = {0.0f};
    f_ptr (x);
}

/*
** fn_caller_0:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	mov	r0, r4
**	mov	r1, r4
**	mov	r2, r4
**	mov	r3, r4
**	vmov.f32	s1, #1.0e\+0
**	vmov.f64	d1, #1.0e\+0
**	vmov.f64	d2, #1.0e\+0
**	vmov.f64	d3, #1.0e\+0
**	vmov.f64	d4, #1.0e\+0
**	vmov.f64	d5, #1.0e\+0
**	vmov.f64	d6, #1.0e\+0
**	vmov.f64	d7, #1.0e\+0
**	bl	__gnu_cmse_nonsecure_call
** ...
*/

void fn_caller_1 (fn_t_1 *f_ptr) {
    union u_t_1 x = {0.0};
    f_ptr (x);
}

/*
** fn_caller_1:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	mov	r0, r4
**	mov	r1, r4
**	mov	r2, r4
**	mov	r3, r4
**	vmov.f64	d1, #1.0e\+0
**	vmov.f64	d2, #1.0e\+0
**	vmov.f64	d3, #1.0e\+0
**	vmov.f64	d4, #1.0e\+0
**	vmov.f64	d5, #1.0e\+0
**	vmov.f64	d6, #1.0e\+0
**	vmov.f64	d7, #1.0e\+0
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
