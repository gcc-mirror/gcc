/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -mmpx" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void
__attribute__((interrupt))
fn1 (void *frame)
{ /* { dg-message "MPX instructions aren't allowed in interrupt service routine" } */
}

void
__attribute__((interrupt))
fn2 (void *frame, uword_t error)
{ /* { dg-message "MPX instructions aren't allowed in exception service routine" } */
}
