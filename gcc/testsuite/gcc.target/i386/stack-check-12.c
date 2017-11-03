/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

__attribute__ ((noreturn)) void exit (int);

__attribute__ ((noreturn)) void
f (void)
{
  asm volatile ("nop" ::: "edi");
  exit (1);
}

/* { dg-final { scan-assembler-not "or\[ql\]" } } */
/* { dg-final { scan-assembler "pushl	%esi" { target ia32 } } } */
/* { dg-final { scan-assembler "popl	%esi" { target ia32 } } }*/
/* { dg-final { scan-assembler "pushq	%rax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "popq	%rax" { target { ! ia32 } } } }*/

