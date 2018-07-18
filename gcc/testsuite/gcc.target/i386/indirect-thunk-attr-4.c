/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -fno-pic" } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch[256];

__attribute__ ((indirect_branch("thunk-inline")))
int
male_indirect_jump (long offset)
{
  dispatch[offset](offset);
  return 0;
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*_?dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler-times "jmp\[ \t\]*\.LIND" 2 } } */
/* { dg-final { scan-assembler-times "call\[ \t\]*\.LIND" 2 } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
/* { dg-final { scan-assembler-not "__x86_indirect_thunk" } } */
