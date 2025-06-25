/* PR middle-end/79173 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fno-stack-protector -masm=att -mapxf" } */
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

static unsigned long
uaddc (unsigned long x, unsigned long y, unsigned long carry_in, unsigned long *carry_out)
{
  unsigned long r;
  unsigned long c1 = __builtin_add_overflow (x, y, &r);
  unsigned long c2 = __builtin_add_overflow (r, carry_in, &r);
  *carry_out = c1 + c2;
  return r;
}

static unsigned long
usubc (unsigned long x, unsigned long y, unsigned long carry_in, unsigned long *carry_out)
{
  unsigned long r;
  unsigned long c1 = __builtin_sub_overflow (x, y, &r);
  unsigned long c2 = __builtin_sub_overflow (r, carry_in, &r);
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

void
bar (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = usubc (p[0], q[0], 0, &c);
  p[1] = usubc (p[1], q[1], c, &c);
  p[2] = usubc (p[2], q[2], c, &c);
  p[3] = usubc (p[3], q[3], c, &c);
}
