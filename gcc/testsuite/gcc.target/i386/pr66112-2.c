/* PR target/66112 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short int
foo (int a, int b)
{
  unsigned short int res;
  a &= 0xffff;
  b &= 0xffff;
  if (__builtin_mul_overflow (a, b, &res))
    res = 0x123;
  return res;
}

short int
bar (int a, int b)
{
  short int res;
  a = (short int) a;
  b = (short int) b;
  if (__builtin_mul_overflow (a, b, &res))
    res = 0x123;
  return res;
}

/* { dg-final { scan-assembler-times "jn?o\[ \t\]" 2 } } */
/* { dg-final { scan-assembler-times "mulw\[ \t\]" 2 } } */
/* { dg-final { scan-assembler-times "imulw\[ \t\]" 1 } } */
