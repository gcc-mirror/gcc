/* { dg-do assemble { target avx512fp16 } } */
/* { dg-options "-mavx512fp16 -O2" } */

short
foo (short a)
{
  register short b __asm ("%xmm1") = a;
  asm volatile ("": "+v" (b));
  register short c __asm ("%xmm2") = b;
  asm volatile ("": "+v" (c));
  return a;
}
