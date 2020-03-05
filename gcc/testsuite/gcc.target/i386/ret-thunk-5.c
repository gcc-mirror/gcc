/* { dg-do compile } */
/* { dg-options "-O2 -mfunction-return=keep" } */

extern void foo (void) __attribute__ ((function_return("thunk")));

void
foo (void)
{
}

/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_return_thunk" } } */
/* { dg-final { scan-assembler {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {call[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler {\tlfence} } } */
