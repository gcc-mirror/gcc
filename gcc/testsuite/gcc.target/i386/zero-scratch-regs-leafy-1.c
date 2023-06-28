/* { dg-do compile } */
/* { dg-options "-O2 -fzero-call-used-regs=leafy -fno-stack-protector -fno-PIC" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler-not "xorl\[ \t\]+%" } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]+%" } } */
