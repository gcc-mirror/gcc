/* { dg-do compile } */
/* { dg-options "-O2" } */

/* The integer variable shift and rotate instructions truncate their
   shift amounts by the datasize.  Make sure that we don't emit a redundant
   masking operation.  */

unsigned
f1 (unsigned x, int y)
{
  return x << (y & 31);
}

unsigned long long
f2 (unsigned long long x, int y)
{
  return x << (y & 63);
}

unsigned long long
f3 (unsigned long long bit_addr, int y)
{
  unsigned long bitnumb = bit_addr & 63;
  return (1LL << bitnumb);
}

unsigned int
f4 (unsigned int x, unsigned int y)
{
  y &= 31;
  return x >> y | (x << (32 - y));
}

unsigned long long
f5 (unsigned long long x, unsigned long long y)
{
  y &= 63;
  return x >> y | (x << (64 - y));
}

unsigned int
f6 (unsigned int x, unsigned int y)
{
  return (x << (32 - (y & 31)));
}

unsigned long long
f7 (unsigned long long x, unsigned long long y)
{
  return (x << (64 - (y & 63)));
}

unsigned long long
f8 (unsigned long long x, unsigned long long y)
{
  return (x << -(y & 63));
}

/* { dg-final { scan-assembler-times "lsl\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "lsl\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "ror\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "ror\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-not "and\tw\[0-9\]+, w\[0-9\]+, 31" } } */
/* { dg-final { scan-assembler-not "and\tx\[0-9\]+, x\[0-9\]+, 63" } } */
/* { dg-final { scan-assembler-not "sub\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
