/* { dg-do compile } */
/* { dg-options "-Os" } */

int test_0(void)
{
  return 4095;
}

int test_1(void)
{
  return 2147483647;
}

int test_2(void)
{
  return -34816;
}

int test_3(void)
{
  return -2049;
}

int test_4(void)
{
  return 2048;
}

int test_5(void)
{
  return 34559;
}

int test_6(void)
{
  return 43680;
}

void test_7(int *p)
{
  *p = -1432354816;
}

/* { dg-final { scan-assembler-not "l32r" } } */
