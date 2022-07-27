/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

void
foo (float *c)
{
  c[0] = 2.3;
  c[1] = 0.0;
}

/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$0x40133333" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$0x00000000" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "movq\[ \\t\]+\\\$1075000115," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xmm" } } */
