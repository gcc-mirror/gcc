/* { dg-do compile } */
/* { dg-options "-O2 -mextra-l32r-costs=3" } */

int test_0(void)
{
  return 134217216;
}

int test_1(void)
{
  return -27604992;
}

int test_2(void)
{
  return -162279;
}

void test_3(int *p)
{
  *p = 192437;
}

/* { dg-final { scan-assembler-not "l32r" } } */
