/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-insns=3 --param max-rtl-if-conversion-unpredictable-cost=100" } */
/* { dg-additional-options "-misel" { target { powerpc*-*-* } } } */
/* { dg-additional-options "-march=z196" { target { s390x-*-* } } } */
/* { dg-additional-options "-mtune-ctrl=^one_if_conv_insn" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-skip-if "Multiple set if-conversion not guaranteed on all subtargets" { "arm*-*-* avr-*-* cris-*-* hppa*64*-*-* visium-*-*" riscv*-*-* msp430-*-* nios2-*-* pru-*-* } }  */
/* { dg-skip-if "" { "s390x-*-*" } { "-m31" } }  */

typedef int word __attribute__((mode(word)));

word
foo (word x, word y, word a)
{
  word i = x;
  word j = y;
  /* Try to make taking the branch likely.  */
  if (__builtin_expect (x > y, 0))
    {
      i = a;
      j = i;
    }
  return i * j;
}
/* { dg-final { scan-rtl-dump "2 true changes made" "ce1" } } */
