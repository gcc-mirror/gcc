/* PR middle-end/79173 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-protector -masm=att" } */
/* { dg-final { scan-assembler-times "addl\t%e\[^\n\r]*, \\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 4\\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 8\\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "adcl\t%e\[^\n\r]*, 12\\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "subl\t%e\[^\n\r]*, \\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 4\\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 8\\\(%\[^\n\r]*\\\)" 1 } } */
/* { dg-final { scan-assembler-times "sbbl\t%e\[^\n\r]*, 12\\\(%\[^\n\r]*\\\)" 1 } } */

#include <x86intrin.h>

void
foo (unsigned int *p, unsigned int *q)
{
  unsigned char c = _addcarry_u32 (0, p[0], q[0], &p[0]);
  c = _addcarry_u32 (c, p[1], q[1], &p[1]);
  c = _addcarry_u32 (c, p[2], q[2], &p[2]);
  _addcarry_u32 (c, p[3], q[3], &p[3]);
}

void
bar (unsigned int *p, unsigned int *q)
{
  unsigned char c = _subborrow_u32 (0, p[0], q[0], &p[0]);
  c = _subborrow_u32 (c, p[1], q[1], &p[1]);
  c = _subborrow_u32 (c, p[2], q[2], &p[2]);
  _subborrow_u32 (c, p[3], q[3], &p[3]);
}
