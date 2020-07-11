/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk-extern " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch;

int
male_indirect_jump (long offset)
{
  dispatch(offset);
  return 0;
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler {movq[ \t]*_dispatch} { target { lp64 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler {movl[ \t]*_dispatch-L[0-9]+\$pb} { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler-not {\t(lfence|pause)} } } */
/* { dg-final { scan-assembler-not {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler-not {call[ \t]*\.?LIND} } } */
