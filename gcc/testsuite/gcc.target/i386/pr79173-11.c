/* PR middle-end/79173 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-times "addq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "subq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "addl\t%e\[^\n\r]*, \\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 4\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 8\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 12\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "subl\t%e\[^\n\r]*, \\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 4\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 8\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 12\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */

void
foo (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = __builtin_addcl (p[0], q[0], 0, &c);
  p[1] = __builtin_addcl (p[1], q[1], c, &c);
  p[2] = __builtin_addcl (p[2], q[2], c, &c);
  p[3] = __builtin_addcl (p[3], q[3], c, &c);
}

void
bar (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = __builtin_subcl (p[0], q[0], 0, &c);
  p[1] = __builtin_subcl (p[1], q[1], c, &c);
  p[2] = __builtin_subcl (p[2], q[2], c, &c);
  p[3] = __builtin_subcl (p[3], q[3], c, &c);
}
