/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16" } */

int
f1 (_Float16 x, _Float16 y)
{
  return x > y;
}

int
f2 (_Float16 x, _Float16 y)
{
  return x < y;
}

/* { dg-final { scan-assembler-times "seta" 2 } } */

int
f3 (_Float16 x, _Float16 y)
{
  return x >= y;
}

int
f4 (_Float16 x, _Float16 y)
{
  return x <= y;
}

/* { dg-final { scan-assembler-times "setnb" 2 } } */

int
f5 (_Float16 x, _Float16 y)
{
  return __builtin_isunordered (x, y);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } }  */
/* { dg-final { scan-assembler-times "xorl" 5 } } */
/* { dg-final { scan-assembler-times "vcomish\[^\n\r\]*xmm\[0-9\]" 4 } } */
