/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test_i0(int x)
{
  return x != 0 ? ~x : ~0;
}

int test_i1(int x)
{
  return x != 1 ? ~x : -2;
}

int test_im1(int x)
{
  return x != ~0 ? ~x : 0;
}

unsigned int test_u0(unsigned int x)
{
  return x != 0 ? ~x : ~0;
}

unsigned int test_u1(unsigned int x)
{
  return x != 1 ? ~x : ~1u;
}

unsigned int test_um1(unsigned int x)
{
  return x != ~0u ? ~x : 0;
}

signed char test_c0(signed char x)
{
  return x != 0 ? ~x : -1;
}

signed char test_c1(signed char x)
{
  return x != 1 ? ~x : -2;
}

signed char test_cm1(signed char x)
{
  return x != -1 ? ~x : 0;
}

signed char test_cm128(signed char x)
{
  return x != -128 ? ~x : 127;
}

signed char test_c127(signed char x)
{
  return x != 127 ? ~x : -128;
}

unsigned char test_uc0(unsigned char x)
{
  return x != 0 ? ~x : 255;
}

unsigned char test_uc1(unsigned char x)
{
  return x != 1 ? ~x : 254;
}

unsigned char test_uc127(unsigned char x)
{
  return x != 127 ? ~x : 128;
}

unsigned char test_uc128(unsigned char x)
{
  return x != 128 ? ~x : 127;
}

unsigned char test_ucm1(unsigned char x)
{
  return x != 255 ? ~x : 0;
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */
