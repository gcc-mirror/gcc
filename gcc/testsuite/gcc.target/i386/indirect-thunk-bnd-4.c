/* { dg-do compile { target { *-*-linux* && { ! x32 } } } } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk -fcheck-pointer-bounds -mmpx -fpic -fno-plt" } */

void bar (char *);
char buf[10];

int
foo (void)
{
  bar (buf);
  return 0;
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*bar@GOT" } } */
/* { dg-final { scan-assembler "bnd call\[ \t\]*__x86_indirect_thunk_bnd_(r|e)ax" } } */
/* { dg-final { scan-assembler-times "bnd call\[ \t\]*\.LIND" 1 } } */
/* { dg-final { scan-assembler "bnd ret" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
