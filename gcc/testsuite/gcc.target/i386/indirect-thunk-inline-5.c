/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -fpic -fno-plt -mindirect-branch=thunk-inline" } */

extern void bar (void);

void
foo (void)
{
  bar ();
}

/* { dg-final { scan-assembler "push(?:l|q)\[ \t\]*bar@GOT" { target x32 } } } */
/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*bar@GOT" { target { ! x32 } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "call\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
/* { dg-final { scan-assembler-not "__x86_indirect_thunk" } } */
