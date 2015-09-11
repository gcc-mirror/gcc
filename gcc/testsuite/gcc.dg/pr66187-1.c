/* PR tree-optimization/66187 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-wrapv" } */

__attribute__((noinline, noclone)) int
f0 (unsigned char x, unsigned char y)
{
  return (x + y) & 0x2ff;
}

__attribute__((noinline, noclone)) int
f1 (unsigned char x, unsigned char y)
{
  return (x - y) & 0x2ff;
}

__attribute__((noinline, noclone)) int
f2 (signed char x, signed char y)
{
  return (x + y) & -4;
}

__attribute__((noinline, noclone)) int
f3 (signed char x, signed char y)
{
  return (x + y) & 0xf8;
}

__attribute__((noinline, noclone)) int
f4 (signed char x, signed char y)
{
  return (x + y) & 0x78;
}

__attribute__((noinline, noclone)) int
f5 (unsigned char x, unsigned char y)
{
  int a = x;
  int b = y;
  int c = a + b;
  return c & 0x2ff;
}

__attribute__((noinline, noclone)) int
f6 (unsigned char x, unsigned char y)
{
  int a = x;
  int b = y;
  int c = a - b;
  return c & 0x2ff;
}

__attribute__((noinline, noclone)) int
f7 (signed char x, signed char y)
{
  int a = x;
  int b = y;
  int c = a + b;
  return c & -4;
}

__attribute__((noinline, noclone)) int
f8 (signed char x, signed char y)
{
  int a = x;
  int b = y;
  int c = a + b;
  return c & 0xf8;
}

__attribute__((noinline, noclone)) int
f9 (signed char x, signed char y)
{
  int a = x;
  int b = y;
  int c = a + b;
  return c & 0x78;
}

int
main ()
{
  if (__SCHAR_MAX__ != 127 || sizeof (int) != 4)
    return 0;
  if (f0 (0xff, 0xff) != 0xfe
      || f1 (0, 1) != 0x2ff
      || f2 (-2, 1) != -4
      || f3 (-2, 1) != 0xf8
      || f4 (-2, 1) != 0x78
      || f5 (0xff, 0xff) != 0xfe
      || f6 (0, 1) != 0x2ff
      || f7 (-2, 1) != -4
      || f8 (-2, 1) != 0xf8
      || f9 (-2, 1) != 0x78)
    __builtin_abort ();
  return 0;
}
