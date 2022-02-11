/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test_i0(int x)
{
  return x != 0 ? -x : 0;
}

int test_i1(int x)
{
  return x != 1 ? -x : -1;
}

int test_im1(int x)
{
  return x != -1 ? -x : 1;
}

unsigned int test_u0(unsigned int x)
{
  return x != 0 ? -x : 0;
}

unsigned int test_u1(unsigned int x)
{
  return x != 1 ? -x : ~0u;
}

unsigned int test_um1(unsigned int x)
{
  return x != ~0u ? -x : 1;
}

unsigned char test_uc0(unsigned char x)
{
  return x != 0 ? -x : 0;
}

unsigned char test_uc1(unsigned char x)
{
  return x != 1 ? -x : 255;
}

unsigned char test_uc127(unsigned char x)
{
  return x != 127 ? -x : 129;
}

unsigned char test_uc128(unsigned char x)
{
  return x != 128 ? -x : 128;
}

unsigned char test_uc255(unsigned char x)
{
  return x != 255 ? -x : 1;
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */
