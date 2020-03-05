/* { dg-do compile } */
/* { dg-options "-O2 -mfunction-return=thunk-inline" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {call[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
/* { dg-final { scan-assembler-not "jmp\[ \t\]*_?__x86_return_thunk" } } */
