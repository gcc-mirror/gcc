/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk -fcheck-pointer-bounds -mmpx" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

void (*dispatch) (char *);
char buf[10];

int
foo (void)
{
  dispatch (buf);
  return 0;
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*_?dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*_?__x86_indirect_thunk_bnd_(r|e)ax" } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*\.?LIND" } } */
/* { dg-final { scan-assembler "bnd ret" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
