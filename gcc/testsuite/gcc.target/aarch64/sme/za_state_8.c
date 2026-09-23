// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sme.h>

void callee_ns();
__arm_streaming __arm_inout("za") void callee_s();

/*
** foo:
**	...
**	smstart	za
**	...
**	msr	tpidr2_el0, x\d+
**	...
*/
__arm_locally_streaming __arm_new("za") const float * foo(const float* x) {
    callee_ns ();
    const float32_t *x_f_in = x;
    svzero_za();
    callee_s ();
    return x_f_in;
}


