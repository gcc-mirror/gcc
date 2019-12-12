/* PR target/88465 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq -mno-avx512bw" } */
/* { dg-final { scan-assembler-times "kxorb\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "kxnorb\[ \t]" 1 } } */

void
foo (void)
{
  unsigned char k = 0;
  __asm volatile ("" : : "k" (k));
  k = -1;
  __asm volatile ("" : : "k" (k));
}
