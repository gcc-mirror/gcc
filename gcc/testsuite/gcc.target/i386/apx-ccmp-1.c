/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapx-features=ccmp" } */

int
f1 (int a)
{
  return a < 17 || a == 32;
}

int
f2 (int a)
{
  return a > 33 || a == 18;
}

int
f3 (int a, int b)
{
  return a != 19 && b > 34;
}

int
f4 (int a, int b)
{
  return a < 35 && b == 20;
}

int
f5 (short a)
{
  return a == 0 || a == 5;
}

int
f6 (long long a)
{
  return a == 6 || a == 0;
}

int
f7 (char a, char b)
{
  return a > 0 && b <= 7;
}

int
f8 (int a, int b)
{
  return a == 9 && b > 0;
}

int
f9 (int a, int b)
{
  a += b;
  return a == 3 || a == 0;
}

/* { dg-final { scan-assembler-times "ccmpg" 2 } } */
/* { dg-final { scan-assembler-times "ccmple" 2 } } */
/* { dg-final { scan-assembler-times "ccmpne" 4 } } */
/* { dg-final { scan-assembler-times "ccmpe" 1 } } */

