// PR middle-end/79173
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-stack-protector -masm=att" }
// { dg-final { scan-assembler-times "addq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 { target lp64 } } }
// { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 { target lp64 } } }
// { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 { target lp64 } } }
// { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 { target lp64 } } }
// { dg-final { scan-assembler-times "addl\t%e\[^\n\r]*, \\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } }
// { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 4\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } }
// { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 8\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } }
// { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 12\\\(%e\[^\n\r]*\\\)" 1 { target ia32 } } }

template <typename T>
inline constexpr T
uaddc (T x, T y, T carry_in, T &carry_out) noexcept
{
  [[gnu::assume (carry_in <= 1)]];
  x += y;
  carry_out = x < y;
  x += carry_in;
  carry_out += x < carry_in;
  return x;
}

void
foo (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = uaddc (p[0], q[0], 0UL, c);
  p[1] = uaddc (p[1], q[1], c, c);
  p[2] = uaddc (p[2], q[2], c, c);
  p[3] = uaddc (p[3], q[3], c, c);
}
