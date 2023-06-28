/* { dg-do compile } */
/* { dg-options "-O2 -fzero-call-used-regs=leafy-gpr -fno-stack-protector -fno-PIC" } */

extern int bar (int);

void
foo (void)
{
  int x = bar (0);
  if (x)
    bar (1);
}

/* { dg-final { scan-assembler "xorl\[ \t\]+%eax, %eax" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%ecx, %ecx" } } */
