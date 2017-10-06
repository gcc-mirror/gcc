/* PR target/82361 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=generic -masm=att -mno-8bit-idiv" } */
/* We should be able to optimize all %eax to %rax zero extensions, because
   div and idiv instructions with 32-bit operands zero-extend both results.   */
/* { dg-final { scan-assembler-not "movl\t%eax, %eax" } } */
/* FIXME: We are still not able to optimize the modulo in f1/f2, only manage
   one.  */
/* { dg-final { scan-assembler-times "movl\t%edx, %edx" 2 } } */

void
f1 (unsigned int a, unsigned int b)
{
  unsigned long long c = a / b;
  unsigned long long d = a % b;
  asm volatile ("" : : "r" (c), "r" (d));
}

void
f2 (int a, int b)
{
  unsigned long long c = (unsigned int) (a / b);
  unsigned long long d = (unsigned int) (a % b);
  asm volatile ("" : : "r" (c), "r" (d));
}

void
f3 (unsigned int a, unsigned int b)
{
  unsigned long long c = a / b;
  asm volatile ("" : : "r" (c));
}

void
f4 (int a, int b)
{
  unsigned long long c = (unsigned int) (a / b);
  asm volatile ("" : : "r" (c));
}

void
f5 (unsigned int a, unsigned int b)
{
  unsigned long long d = a % b;
  asm volatile ("" : : "r" (d));
}

void
f6 (int a, int b)
{
  unsigned long long d = (unsigned int) (a % b);
  asm volatile ("" : : "r" (d));
}
