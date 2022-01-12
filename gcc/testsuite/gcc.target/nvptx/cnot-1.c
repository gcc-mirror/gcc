/* { dg-do compile } */
/* { dg-options "-O2" } */

int test1(int x)
{
  return !x;
}

int test2(int x)
{
  return x ? 0 : 1;
}

int test3(int x)
{
  return (x == 0) ? 1 : 0;
}

unsigned int test4(unsigned int x)
{
  return !x;
}

unsigned int test5(unsigned int x)
{
  return x ? 0 : 1;
}

unsigned int test6(unsigned int x)
{
  return (x == 0) ? 1 : 0;
}

short test7(short x)
{
  return !x;
}

short test8(short x)
{
  return x ? 0 : 1;
}

short test9(short x)
{
  return (x == 0) ? 1 : 0;
}

unsigned short test10(unsigned short x)
{
  return !x;
}

unsigned short test11(unsigned short x)
{
  return x ? 0 : 1;
}

unsigned short test12(unsigned short x)
{
  return (x == 0) ? 1 : 0;
}

long test13(long x)
{
  return !x;
}

long test14(long x)
{
  return x ? 0 : 1;
}

long test15(long x)
{
  return (x == 0) ? 1: 0;
}

unsigned long test16(unsigned long x)
{
  return !x;
}

unsigned long test17(unsigned long x)
{
  return x ? 0 : 1;
}

unsigned long test18(unsigned long x)
{
  return (x == 0) ? 1 : 0;
}

/* { dg-final { scan-assembler-times "cnot.b" 18 } } */
