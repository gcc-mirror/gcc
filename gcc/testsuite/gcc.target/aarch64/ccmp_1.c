/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f1 (int a)
{
  return a == 17 || a == 32;
}

int
f2 (int a)
{
  return a == 33 || a == 18;
}

int
f3 (int a, int b)
{
  return a == 19 && b == 34;
}

int
f4 (int a, int b)
{
  return a == 35 && b == 20;
}

int
f5 (int a)
{
  return a == 0 || a == 5;
}

int
f6 (int a)
{
  return a == 6 || a == 0;
}

int
f7 (int a, int b)
{
  return a == 0 && b == 7;
}

int
f8 (int a, int b)
{
  return a == 9 && b == 0;
}

int
f9 (float a, float b)
{
  return a < 0.0f && a > b;
}

int
f10 (float a, float b)
{
  return a == b || b == 0.0f;
}

int
f11 (double a, int b)
{
  return a < 0.0f && b == 30;
}

int
f12 (double a, int b)
{
  return b == 31 || a == 0.0f;
}

int
f13 (int a, int b)
{
  a += b;
  return a == 3 || a == 0;
}

/* { dg-final { scan-assembler "fccmp\t" } } */
/* { dg-final { scan-assembler "fccmpe\t" } } */
