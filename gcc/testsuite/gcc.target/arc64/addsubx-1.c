/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Checks if the compiler generates shifted adds and subs.  */

int foo (int a, int b)
{
  return a + (b << 1);
}

long int bar (long int a, long int b)
{
  return a + (b << 1);
}

int minus1 (int a, int b)
{
  return a - (b << 1);
}

long int minus1l (long int a, long int b)
{
  return a - (b << 1);
}

int plus1 (int a, int b)
{
  int x = a + (b << 3);
  if (x != 0)
    return x;
  return 20;
}

/* { dg-final { scan-assembler "add1_s\\s+" } } */
/* { dg-final { scan-assembler "add1l\\s+" { target { hs6x } } } } */
/* { dg-final { scan-assembler "sub1\\s+" } } */
/* { dg-final { scan-assembler "sub1l\\s+" { target { hs6x } } } } */
/* { dg-final { scan-assembler "add3\\.f\\s+" } } */
