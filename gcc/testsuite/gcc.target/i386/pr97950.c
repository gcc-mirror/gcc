/* PR target/95950 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "\tset\[ab]\t" 4 } } */
/* { dg-final { scan-assembler-times "\tseto\t" 16 } } */
/* { dg-final { scan-assembler-times "\tsetc\t" 4 } } */
/* { dg-final { scan-assembler-not "\tjn?\[ab]\t" } } */
/* { dg-final { scan-assembler-not "\tjn?o\t" } } */
/* { dg-final { scan-assembler-not "\tjn?c\t" } } */

char
f1 (short a, short b)
{
  return __builtin_mul_overflow_p (a, b, (short) 0);
}

char
f2 (short a, short b)
{
  return __builtin_add_overflow_p (a, b, (short) 0);
}

char
f3 (short a, short b)
{
  return __builtin_sub_overflow_p (a, b, (short) 0);
}

char
f4 (unsigned short a, unsigned short b)
{
  return __builtin_mul_overflow_p (a, b, (unsigned short) 0);
}

char
f5 (unsigned short a, unsigned short b)
{
  return __builtin_add_overflow_p (a, b, (unsigned short) 0);
}

char
f6 (unsigned short a, unsigned short b)
{
  return __builtin_sub_overflow_p (a, b, (unsigned short) 0);
}

char
f7 (short a, short b)
{
  return __builtin_mul_overflow_p (a, b, (short) 0);
}

char
f8 (short a, short b)
{
  return __builtin_add_overflow_p (a, b, (short) 0);
}

char
f9 (short a, short b)
{
  return __builtin_sub_overflow_p (a, b, (short) 0);
}

char
f10 (unsigned short a, unsigned short b)
{
  return __builtin_mul_overflow_p (a, b, (unsigned short) 0);
}

char
f11 (unsigned short a, unsigned short b)
{
  return __builtin_add_overflow_p (a, b, (unsigned short) 0);
}

char
f12 (unsigned short a, unsigned short b)
{
  return __builtin_sub_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f13 (short a, short b)
{
  return __builtin_mul_overflow_p (a, b, (short) 0);
}

unsigned short
f14 (short a, short b)
{
  return __builtin_add_overflow_p (a, b, (short) 0);
}

unsigned short
f15 (short a, short b)
{
  return __builtin_sub_overflow_p (a, b, (short) 0);
}

unsigned short
f16 (unsigned short a, unsigned short b)
{
  return __builtin_mul_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f17 (unsigned short a, unsigned short b)
{
  return __builtin_add_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f18 (unsigned short a, unsigned short b)
{
  return __builtin_sub_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f19 (short a, short b)
{
  return __builtin_mul_overflow_p (a, b, (short) 0);
}

unsigned short
f20 (short a, short b)
{
  return __builtin_add_overflow_p (a, b, (short) 0);
}

unsigned short
f21 (short a, short b)
{
  return __builtin_sub_overflow_p (a, b, (short) 0);
}

unsigned short
f22 (unsigned short a, unsigned short b)
{
  return __builtin_mul_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f23 (unsigned short a, unsigned short b)
{
  return __builtin_add_overflow_p (a, b, (unsigned short) 0);
}

unsigned short
f24 (unsigned short a, unsigned short b)
{
  return __builtin_sub_overflow_p (a, b, (unsigned short) 0);
}
