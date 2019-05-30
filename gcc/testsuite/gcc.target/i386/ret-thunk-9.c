/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=thunk -mindirect-branch=thunk " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*bar) (void);

int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_return_thunk" } } */
/* { dg-final { scan-assembler {jmp[ \t]+\.?LIND} } } */
/* { dg-final { scan-assembler {call[ \t]+\.?LIND} } } */
/* { dg-final { scan-assembler "_?__x86_return_thunk:" } } */
/* { dg-final { scan-assembler "mov(?:l|q)\[ \t\]*bar" { target *-*-linux* } } } */
/* { dg-final { scan-assembler {movq[ \t]*_bar} { target { lp64 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler {movl[ \t]*l_bar\$non_lazy_ptr-L[0-9]+\$pb} { target { ia32 && *-*-darwin* } } } } */
/* { dg-final { scan-assembler-times {\tpause} 2 } } */
/* { dg-final { scan-assembler-times {\tlfence} 2 } } */
/* { dg-final { scan-assembler "call\[ \t\]*_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler-not "pushq\[ \t\]%rax" } } */
