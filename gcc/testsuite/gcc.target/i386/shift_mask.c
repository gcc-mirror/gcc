/* { dg-do compile } */
/* { dg-options "-O2" } */

int test_sal (int a, int c)
{
  return a << (c & 0x1f);
}

int test_sar (int a, int c)
{
  return a >> (c & 0x1f);
}

unsigned int test_shr (unsigned int a, int c)
{
  return a >> (c & 0x1f);
}

unsigned int test_rol (unsigned int a, int c)
{
  int z = c & 0x1f;
  return (a << z) | (a >> (32 - z));
}

unsigned int test_ror (unsigned int a, int c)
{
  int z = c & 0x1f;
  return (a >> z) | (a << (32 - z));
}

/* { dg-final { scan-assembler-not "and" } } */
