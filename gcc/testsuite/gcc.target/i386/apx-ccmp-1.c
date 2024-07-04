/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ffast-math -mapx-features=ccmp" } */

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

int
f10 (float a, int b, float c)
{
  return a > c || b < 19;
}

int
f11 (float a, int b)
{
  return a == 0.0 && b > 21;
}

int
f12 (double a, int b)
{
  return a < 3.0 && b != 23;
}

int
f13 (double a, double b, int c, int d)
{
  a += b;
  c += d;
  return a != b || c == d;
}

int
f14 (double a, int b)
{
  return b != 0 && a < 1.5;
}

int
f15 (double a, double b, int c, int d)
{
  return c != d || a <= b;
}

/* { dg-final { scan-assembler-times "ccmpg" 2 } } */
/* { dg-final { scan-assembler-times "ccmple" 2 } } */
/* { dg-final { scan-assembler-times "ccmpne" 2 } } */
/* { dg-final { scan-assembler-times "ccmpe" 1 } } */
/* { dg-final { scan-assembler-times "ccmpbe" 1 } } */
/* { dg-final { scan-assembler-times "ctestne" 2 } } */
/* { dg-final { scan-assembler-times "cteste" 2 } } */
/* { dg-final { scan-assembler-times "ccmpa" 1 } } */
/* { dg-final { scan-assembler-times "ccmpbl" 1 } } */
/* { dg-final { scan-assembler-times "ctestbl" 1 } } */
