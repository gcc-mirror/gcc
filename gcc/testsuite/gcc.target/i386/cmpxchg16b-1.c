/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mcx16" } */

typedef int TItype __attribute__ ((mode (TI)));

TItype m_128;

void test(TItype x_128)
{
  m_128 = __sync_val_compare_and_swap (&m_128, x_128, m_128);
}

/* { dg-final { scan-assembler "cmpxchg16b\[ \\t]" } } */
