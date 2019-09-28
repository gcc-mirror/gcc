/* { dg-do compile } */
/* { dg-options "-O2 -mno-indirect-branch-register -mfunction-return=keep -mindirect-branch=thunk-inline " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*bar) (void);
extern int foo (void) __attribute__ ((function_return("thunk")));

int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_return_thunk" } } */
/* { dg-final { scan-assembler-times {\tpause} 2 } } */
/* { dg-final { scan-assembler-times {\tlfence} 2 } } */
/* { dg-final { scan-assembler-times {jmp[ \t]*\.?LIND} 3 } } */
/* { dg-final { scan-assembler-times {call[ \t]*\.?LIND} 3 } } */
/* { dg-final { scan-assembler-not "jmp\[ \t\]*_?__x86_indirect_thunk" } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler-not "pushq\[ \t\]%rax" } } */
