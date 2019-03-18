/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -m80387" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void
__attribute__((interrupt))
fn1 (void *frame, uword_t error)
{ /* { dg-message "80387 instructions aren't allowed in an exception service routine" } */
}

void
__attribute__((interrupt))
fn2 (void *frame)
{ /* { dg-message "80387 instructions aren't allowed in an interrupt service routine" } */
}
