/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int test_ior(unsigned char i)
{
  return i | (i<<8) | (i<<16) | (i<<24);
}

unsigned int test_xor(unsigned char i)
{
  return i ^ (i<<8) ^ (i<<16) ^ (i<<24);
}

unsigned int test_ior_1s(unsigned char i)
{
  return i | (i<<8);
}

unsigned int test_ior_1u(unsigned char i)
{
  unsigned int t = i;
  return t | (t<<8);
}

unsigned int test_xor_1s(unsigned char i)
{
  return i ^ (i<<8);
}

unsigned int test_xor_1u(unsigned char i)
{
  unsigned int t = i;
  return t ^ (t<<8);
}

unsigned int test_ior_2s(unsigned char i)
{
  return (i<<8) | (i<<16);
}

unsigned int test_ior_2u(unsigned char i)
{
  unsigned int t = i;
  return (t<<8) | (t<<16);
}

unsigned int test_xor_2s(unsigned char i)
{
  return (i<<8) ^ (i<<16);
}

unsigned int test_xor_2u(unsigned char i)
{
  unsigned int t = i;
  return (t<<8) ^ (t<<16);
}

/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* 16843009" 2 "optimized" } } */

