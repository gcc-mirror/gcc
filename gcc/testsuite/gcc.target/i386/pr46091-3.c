/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

unsigned long long test (unsigned long long a)
{
  return a ^ (1ull << 55);
}

/* { dg-final { scan-assembler "btc" } } */
