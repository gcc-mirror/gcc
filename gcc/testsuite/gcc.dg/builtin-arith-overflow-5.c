/* PR rtl-optimization/95862 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f1 (int a, int b)
{
  unsigned long long c;
  return __builtin_mul_overflow (a, b, &c);
}

int
f2 (int a, unsigned b)
{
  unsigned long long c;
  return __builtin_mul_overflow (a, b, &c);
}

int
f3 (unsigned a, unsigned b)
{
  long long c;
  return __builtin_mul_overflow (a, b, &c);
}

int
f4 (int a, unsigned b)
{
  long long c;
  return __builtin_mul_overflow (a, b, &c);
}

short
f5 (short a, short b)
{
  unsigned c;
  return __builtin_mul_overflow (a, b, &c);
}

short
f6 (short a, unsigned short b)
{
  unsigned c;
  return __builtin_mul_overflow (a, b, &c);
}

short
f7 (unsigned short a, unsigned short b)
{
  int c;
  return __builtin_mul_overflow (a, b, &c);
}

short
f8 (short a, unsigned short b)
{
  int c;
  return __builtin_mul_overflow (a, b, &c);
}

signed char
f9 (signed char a, signed char b)
{
  unsigned short c;
  return __builtin_mul_overflow (a, b, &c);
}

signed char
f10 (signed char a, unsigned char b)
{
  unsigned short c;
  return __builtin_mul_overflow (a, b, &c);
}

signed char
f11 (unsigned char a, unsigned char b)
{
  short c;
  return __builtin_mul_overflow (a, b, &c);
}

signed char
f12 (signed char a, unsigned char b)
{
  short c;
  return __builtin_mul_overflow (a, b, &c);
}
