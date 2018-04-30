/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk -fcheck-pointer-bounds -mmpx -fno-pic" } */

void (*dispatch) (char *);
char buf[10];

void
foo (void)
{
  dispatch (buf);
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*_?dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler "bnd jmp\[ \t\]*__x86_indirect_thunk_bnd_rax" { target lp64 } } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*__x86_indirect_thunk_bnd_eax" { target ia32 } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "bnd ret" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
