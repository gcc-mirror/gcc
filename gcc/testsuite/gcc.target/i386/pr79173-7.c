/* PR middle-end/79173 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-times "addq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "subq\t%r\[^\n\r]*, \\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 8\\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 16\\\(%rdi\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbq\t%r\[^\n\r]*, 24\\\(%rdi\\\)" 1 } } */

#include <x86intrin.h>

void
foo (unsigned long long *p, unsigned long long *q)
{
  unsigned char c = _addcarry_u64 (0, p[0], q[0], &p[0]);
  c = _addcarry_u64 (c, p[1], q[1], &p[1]);
  c = _addcarry_u64 (c, p[2], q[2], &p[2]);
  _addcarry_u64 (c, p[3], q[3], &p[3]);
}

void
bar (unsigned long long *p, unsigned long long *q)
{
  unsigned char c = _subborrow_u64 (0, p[0], q[0], &p[0]);
  c = _subborrow_u64 (c, p[1], q[1], &p[1]);
  c = _subborrow_u64 (c, p[2], q[2], &p[2]);
  _subborrow_u64 (c, p[3], q[3], &p[3]);
}
