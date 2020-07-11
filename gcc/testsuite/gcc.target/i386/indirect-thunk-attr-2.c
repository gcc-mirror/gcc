/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch[256];

__attribute__ ((indirect_branch("thunk")))
void
male_indirect_jump (long offset)
{
  dispatch[offset](offset);
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler {leaq[ \t]*_dispatch} { target { lp64 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler {movl[ \t]*_dispatch-L[0-9]+\$pb} { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {call[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
