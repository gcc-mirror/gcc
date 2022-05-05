/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mf16c -mno-avx512fp16" } */
/* { dg-final { scan-assembler-times "vmovd" 2 } } */
/* { dg-final { scan-assembler-not "\\\(%rsp\\\)"} } */
short test (_Float16 a)
{
  union{
    short b;
    _Float16 a;}u;
  u.a = a;
  return u.b;
}

_Float16 test1 (short a)
{
  union{
    _Float16 b;
    short a;}u;
  u.a = a;
  return u.b;
}
