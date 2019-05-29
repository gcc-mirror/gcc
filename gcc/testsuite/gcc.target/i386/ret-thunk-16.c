/* { dg-do compile } */
/* { dg-options "-O2 -mfunction-return=thunk-inline -mindirect-branch=thunk-extern " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*bar) (void);

__attribute__ ((function_return("keep"), indirect_branch("keep")))
int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler-not "_?__x86_indirect_thunk" } } */
/* { dg-final { scan-assembler-not "_?__x86_return_thunk" } } */
/* { dg-final { scan-assembler-not {\t(lfence|pause)} } } */
/* { dg-final { scan-assembler-not {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler-not {call[ \t]*\.?LIND} } } */
