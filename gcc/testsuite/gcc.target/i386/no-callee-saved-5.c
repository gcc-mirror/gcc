/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef void (*fn_t) (void *) __attribute__ ((no_callee_saved_registers));

void
foo (void *frame)
{
}

fn_t func = foo; /* { dg-error "incompatible pointer type" } */
