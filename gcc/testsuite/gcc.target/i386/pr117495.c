/* PR target/117495 */
/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -fno-trapping-math" } */
/* { dg-final { scan-assembler-times "vcomisbf16" 2 } } */

__attribute__((target("avx10.2-256")))
int foo (int b, int x)
{
  return (__bf16) b < x;
}

int foo2 (int b, int x)
{
  return (__bf16) b < x;
}

__attribute__((target("avx10.2-256")))
int foo3 (__bf16 b, __bf16 x)
{
  return (__bf16) b < x;
}

int foo4 (__bf16 b, __bf16 x)
{
  return (__bf16) b < x;
}
