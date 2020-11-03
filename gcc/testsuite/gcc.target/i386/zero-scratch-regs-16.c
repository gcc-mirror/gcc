/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all" } */

extern void foo (void) __attribute__ ((zero_call_used_regs("skip")));

void
foo (void)
{
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler-not "xorl\[ \t\]+%" } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]+%" } } */
