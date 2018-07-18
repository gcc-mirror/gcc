/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

long test_1 (long x, int n)
{
  x &= ~((long)0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "btr" } } */

long test_2 (long x, int n)
{
  x |= ((long)0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "bts" } } */

long test_3 (long x, int n)
{
  x ^= ((long)0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "btc" } } */
