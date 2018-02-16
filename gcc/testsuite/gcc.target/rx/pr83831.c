/* { dg-do compile }  */
/* { dg-options "-O1" }  */
/* { dg-final { scan-assembler-times "bclr" 6 } }  */
/* { dg-final { scan-assembler-times "bset" 7 } }  */
/* { dg-final { scan-assembler-times "bnot" 7 } }  */

void
test_0 (char* x, unsigned int y)
{
  /* Expect 4x bclr here.  */
  x[0] &= 0xFE;
  x[1] = y & ~(1 << 1);
  x[2] &= 0xFE;
  x[65000] &= 0xFE;
}

unsigned int
test_1 (unsigned int x, unsigned int y)
{
  /* Expect 1x bclr here.  */
  return x & ~(1 << y);
}

unsigned int
test_2 (unsigned int x)
{
  /* Expect 1x bclr here.  */
  return x & ~(1 << 1);
}

void
test_3 (char* x, unsigned int y, unsigned int z)
{
  /* Expect 5x bset here.  */
  x[0] |= 0x10;
  x[1] = y | (1 << 1);
  x[2] |= 0x10;
  x[65000] |= 0x10;
  x[5] |= 1 << z;
}

unsigned int
test_4 (unsigned int x, unsigned int y)
{
  /* Expect 1x bset here.  */
  return x | (1 << y);
}

unsigned int
test_5 (unsigned int x)
{
  /* Expect 1x bset here.  */
  return x | (1 << 8);
}

void
test_6 (char* x, unsigned int y, unsigned int z)
{
  /* Expect 5x bnot here.  */
  x[0] ^= 0x10;
  x[1] = y ^ (1 << 1);
  x[2] ^= 0x10;
  x[65000] ^= 0x10;
  x[5] ^= 1 << z;
}

unsigned int
test_7 (unsigned int x, unsigned int y)
{
  /* Expect 1x bnot here.  */
  return x ^ (1 << y);
}

unsigned int
test_8 (unsigned int x)
{
  /* Expect 1x bnot here.  */
  return x ^ (1 << 8);
}
