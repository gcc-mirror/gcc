/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

long test_1 (long x, int n)
{
  n &= 0x3f;

  x &= ~((long)0x01 << n);

  return x;
}

long test_2 (long x, int n)
{
  n &= 0x3f;

  x |= ((long)0x01 << n);

  return x;
}

long test_3 (long x, int n)
{
  n &= 0x3f;

  x ^= ((long)0x01 << n);

  return x;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
