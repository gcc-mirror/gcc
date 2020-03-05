/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=keep " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*func_p) (void);

void
foo (void)
{
  asm("call __x86_indirect_thunk_%V0" : : "a" (func_p));
}

/* { dg-final { scan-assembler "call\[ \t\]*__x86_indirect_thunk_eax" { target ia32 } } } */
/* { dg-final { scan-assembler "call\[ \t\]*__x86_indirect_thunk_rax" { target { ! ia32 } } } } */
