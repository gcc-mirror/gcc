/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -fno-pic" } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch[256];

__attribute__ ((indirect_branch("thunk-extern")))
int
male_indirect_jump (long offset)
{
  dispatch[offset](offset);
  return 0;
}

/* { dg-final { scan-assembler "push(?:l|q)\[ \t\]*_?dispatch" { target { { ! x32 } && *-*-linux* } } } } */
/* { dg-final { scan-assembler-times "jmp\[ \t\]*\.LIND" 1 { target { ! x32 } } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]*\.LIND" 1 { target { ! x32 } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*__x86_indirect_thunk" { target { ! x32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*__x86_indirect_thunk_(r|e)ax" { target x32 } } } */
/* { dg-final { scan-assembler-not {\t(lfence|pause)} } } */
