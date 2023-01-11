/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int test1(unsigned int a , unsigned int b)
{
  return (a << 2) + (b << 2) == a * 4 + b * 4;
}

unsigned int test2(unsigned int a , unsigned int b)
{
  return (a << 2) + (b << 2) == (a + b) << 2;
}

unsigned int test3(unsigned int a , unsigned int b)
{
  return a * 4 + b * 4 == (a + b) * 4;
}

unsigned int test4(unsigned int a , unsigned int b)
{
  return (a + b) << 2 == (a + b) * 4;
}

unsigned int test5(unsigned int a , unsigned int b)
{
  return (a << 2) + (b << 2) ==  (a + b) * 4;
}

unsigned int test6(unsigned int a , unsigned int b)
{
  return (a + b) << 2 == a * 4 + b * 4;
}

/* { dg-final { scan-tree-dump-times "return 1" 6 "optimized" } } */
