/* PR middle-end/79173 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-times "addq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "addl\t%e\[^\n\r]*, \\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 4\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 8\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 12\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } } */

static unsigned long
uaddc (unsigned long x, unsigned long y, unsigned long carry_in, unsigned long *carry_out)
{
  unsigned long r = x + y;
  unsigned long c1 = r < x;
  r += carry_in;
  unsigned long c2 = r < carry_in;
  *carry_out = c1 + c2;
  return r;
}

void
foo (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = uaddc (p[0], q[0], 0, &c);
  p[1] = uaddc (p[1], q[1], c, &c);
  p[2] = uaddc (p[2], q[2], c, &c);
  p[3] = uaddc (p[3], q[3], c, &c);
}
