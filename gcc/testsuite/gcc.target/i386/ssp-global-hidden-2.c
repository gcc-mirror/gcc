/* { dg-do compile { target { fstack_protector && fpic } } } */
/* { dg-options "-O2 -fPIC -fstack-protector-all -mstack-protector-guard=global" } */

#include <stdint.h>

extern uintptr_t __stack_chk_guard;
__attribute__ ((visibility ("hidden")))
extern uintptr_t __stack_chk_guard;

void
smash (char *p, int i)
{
  p[i] = 42;
}

/* { dg-final { scan-hidden "__stack_chk_guard" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "__stack_chk_guard\\(%rip\\)" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "__stack_chk_guard@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "__stack_chk_guard@GOTOFF" { target { ia32 && { ! *-*-darwin* } } } } } */
/* { dg-final { scan-assembler-not "__stack_chk_guard@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler {L___stack_chk_guard\$non_lazy_ptr-L.\$pb} { target { ia32 && *-*-darwin* } } } } */
