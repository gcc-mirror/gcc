/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */

int test_1 (int x, int n)
{
  x &= ~(0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "btr" } } */

int test_2 (int x, int n)
{
  x |= (0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "bts" } } */

int test_3 (int x, int n)
{
  x ^= (0x01 << n);

  return x;
}

/* { dg-final { scan-assembler "btc" } } */
