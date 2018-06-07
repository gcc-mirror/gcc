/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -fpic -fno-plt -mindirect-branch=thunk" } */

extern void bar (void);

int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler "push(?:l|q)\[ \t\]*bar@GOT" { target x32 } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*__x86_indirect_thunk" { target x32 } } } */
/* { dg-final { scan-assembler-times "jmp\[ \t\]*\.LIND" 2 { target x32 } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]*\.LIND" 2 { target x32 } } } */
/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*bar@GOT" { target { ! x32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*__x86_indirect_thunk_(r|e)ax" { target { ! x32 } } } } */
/* { dg-final { scan-assembler-times "jmp\[ \t\]*\.LIND" 1 { target { ! x32 } } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]*\.LIND" 1 { target { ! x32 } } } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
