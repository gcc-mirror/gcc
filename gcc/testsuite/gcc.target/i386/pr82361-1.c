/* PR target/82361 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=generic -masm=att -mno-8bit-idiv" } */
/* We should be able to optimize all %eax to %rax zero extensions, because
   div and idiv instructions with 32-bit operands zero-extend both results.   */
/* { dg-final { scan-assembler-not "movl\t%eax, %eax" } } */
/* FIXME: The compiler does not merge zero-extension to the modulo part
   of f1 and f2.  */
/* { dg-final { scan-assembler-times "movl\t%edx" 2 } } */

void
f1 (unsigned int a, unsigned int b)
{
  register unsigned long long c asm ("rax") = a / b;
  register unsigned long long d asm ("rdx") = a % b;
  asm volatile ("" : : "r" (c), "r" (d));
}

void
f2 (int a, int b)
{
  register unsigned long long c asm ("rax") = (unsigned int) (a / b);
  register unsigned long long d asm ("rdx") = (unsigned int) (a % b);
  asm volatile ("" : : "r" (c), "r" (d));
}

void
f3 (unsigned int a, unsigned int b)
{
  register unsigned long long c asm ("rax") = a / b;
  asm volatile ("" : : "r" (c));
}

void
f4 (int a, int b)
{
  register unsigned long long c asm ("rax") = (unsigned int) (a / b);
  asm volatile ("" : : "r" (c));
}

void
f5 (unsigned int a, unsigned int b)
{
  register unsigned long long d asm ("rdx") = a % b;
  asm volatile ("" : : "r" (d));
}

void
f6 (int a, int b)
{
  register unsigned long long d asm ("rdx") = (unsigned int) (a % b);
  asm volatile ("" : : "r" (d));
}
