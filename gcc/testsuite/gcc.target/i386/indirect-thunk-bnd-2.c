/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk -fcheck-pointer-bounds -mmpx -fno-pic" } */

void (*dispatch) (char *);
char buf[10];

int
foo (void)
{
  dispatch (buf);
  return 0;
}

/* { dg-final { scan-assembler "push(?:l|q)\[ \t\]*_?dispatch" { target { { ! x32 } && *-*-linux* } } } } */
/* { dg-final { scan-assembler "pushq\[ \t\]%rax" { target x32 } } } */
/* { dg-final { scan-assembler "bnd jmp\[ \t\]*__x86_indirect_thunk_bnd" } } */
/* { dg-final { scan-assembler "bnd jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler "bnd ret" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
