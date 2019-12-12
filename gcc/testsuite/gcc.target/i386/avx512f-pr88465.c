/* PR target/88465 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512dq -mno-avx512bw" } */
/* { dg-final { scan-assembler-times "kxorw\[ \t]" 2 } } */
/* { dg-final { scan-assembler-times "kxnorw\[ \t]" 1 } } */

void
foo (void)
{
  unsigned short int k = 0;
  __asm volatile ("" : : "k" (k));
  k = -1;
  __asm volatile ("" : : "k" (k));
}

void
bar (void)
{
  unsigned char k = 0;
  __asm volatile ("" : : "k" (k));
}
