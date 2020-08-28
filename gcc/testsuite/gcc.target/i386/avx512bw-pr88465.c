/* PR target/88465 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-final { scan-assembler-times "kxor\[qd\]\[ \t]" 2 } } */
/* { dg-final { scan-assembler-times "kxnor\[dq\]\[ \t]" 2 } } */

void
foo (void)
{
  unsigned int k = 0;
  __asm volatile ("" : : "k" (k));
  k = -1;
  __asm volatile ("" : : "k" (k));
}

void
bar (void)
{
  unsigned long long k = 0;
  __asm volatile ("" : : "k" (k));
  k = -1;
  __asm volatile ("" : : "k" (k));
}
