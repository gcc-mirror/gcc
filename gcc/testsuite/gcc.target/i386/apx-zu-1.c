/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "setle"} } */
/* { dg-final { scan-assembler-not "setge"} } */
/* { dg-final { scan-assembler-not "sete"} } */
/* { dg-final { scan-assembler-not "xor"} } */
/* { dg-final { scan-assembler-times "setzune" 1} } */
/* { dg-final { scan-assembler-times "setzule" 1} } */
/* { dg-final { scan-assembler-times "setzue" 1} } */
/* { dg-final { scan-assembler-times "setzuge" 1} } */
/* { dg-final { scan-assembler "imulzu"} } */

__attribute__((noinline, noclone, target("apxf")))
long long foo0 (int a)
{
  return a == 0 ? 0 : 1;
}

__attribute__((noinline, noclone, target("apxf")))
long foo1 (int a, int b)
{
  return a > b ? 0 : 1;
}

__attribute__((noinline, noclone, target("apxf")))
int foo2 (int a, int b)
{
  return a != b ? 0 : 1;
}

__attribute__((noinline, noclone, target("apxf")))
short foo3 (int a, int b)
{
  return a < b ? 0 : 1;
}

__attribute__((noinline, noclone, target("apxf")))
unsigned long
f1(unsigned short x)
{
  unsigned short a;
  a = x * 1000;
  return a;
}
