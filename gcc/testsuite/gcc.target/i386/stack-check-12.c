/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic -fomit-frame-pointer" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

__attribute__ ((noreturn)) void exit (int);

__attribute__ ((noreturn)) void
f (void)
{
  exit (1);
}

/* { dg-final { scan-assembler-not "or\[ql\]" } } */
/* On ia32 PIC, saving the PIC register requires a stack frame, which does away
   with the need for the dummy %esi pushing and popping for stack probing.  */
/* { dg-final { scan-assembler "pushl	%esi" { target { ia32 && nonpic } } } } */
/* { dg-final { scan-assembler "popl	%esi" { target { ia32 && nonpic } } } } */
/* { dg-final { scan-assembler "pushq	%rax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "popq	%rax" { target { ! ia32 } } } } */
