/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch;
extern int male_indirect_jump (long)
  __attribute__ ((indirect_branch("thunk-inline")));

int
male_indirect_jump (long offset)
{
  dispatch(offset);
  return 0;
}

/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*dispatch" { target *-*-linux* } } } */
/* { dg-final { scan-assembler {movq[ \t]*_dispatch} { target { lp64 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler {movl[ \t]*l_dispatch\$non_lazy_ptr-L[0-9]+\$pb} { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times {jmp[ \t]*\.?LIND} 2 } } */
/* { dg-final { scan-assembler-times {call[ \t]*\.?LIND} 2 } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
/* { dg-final { scan-assembler-not "_?__x86_indirect_thunk" } } */
