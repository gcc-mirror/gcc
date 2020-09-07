/* { dg-do compile } */
/* { dg-options "-O2" } */

int
cmp_si_test1 (int a, int b, int c)
{
  if (a > b)
    return a + c;
  else
    return a + b + c;
}

int
cmp_si_test2 (int a, int b, int c)
{
  if ((a >> 3) > b)
    return a + c;
  else
    return a + b + c;
}

typedef long long s64;

s64
cmp_di_test1 (s64 a, s64 b, s64 c)
{
  if (a > b)
    return a + c;
  else
    return a + b + c;
}

s64
cmp_di_test2 (s64 a, s64 b, s64 c)
{
  if ((a >> 3) > b)
    return a + c;
  else
    return a + b + c;
}

int
cmp_di_test3 (int a, s64 b, s64 c)
{
  if (a > b)
    return a + c;
  else
    return a + b + c;
}

int
cmp_di_test4 (int a, s64 b, s64 c)
{
  if (((s64)a << 3) > b)
    return a + c;
  else
    return a + b + c;
}

/* { dg-final { scan-assembler-times "cmp\tw\[0-9\]+, w\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "cmp\tx\[0-9\]+, x\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "cmp\tx\[0-9\]+, w\[0-9\]+, sxtw" 2 } } */
