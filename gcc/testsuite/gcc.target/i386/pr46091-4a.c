/* { dg-do compile } */
/* { dg-options "-O2" } */

int test_1 (int x, int n)
{
  n &= 0x1f;

  x &= ~(0x01 << n);

  return x;
}

int test_2 (int x, int n)
{
  n &= 0x1f;

  x |= (0x01 << n);

  return x;
}

int test_3 (int x, int n)
{
  n &= 0x1f;

  x ^= (0x01 << n);

  return x;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
