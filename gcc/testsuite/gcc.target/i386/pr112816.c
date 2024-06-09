/* PR target/112816 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -masm=att" } */
/* { dg-final { scan-assembler-times "psrad\t\\\$31," 2 } } */
/* { dg-final { scan-assembler-not "pcmpeqd\t" } } */

#define N 4
struct S { float x[N]; };
struct T { int x[N]; };

__attribute__((target ("no-sse3,sse2"))) struct T
foo (struct S x)
{
  struct T res;
  for (int i = 0; i < N; ++i)
    res.x[i] = __builtin_signbit (x.x[i]) ? -1 : 0;
  return res;
}

__attribute__((target ("avx2"))) struct T
bar (struct S x)
{
  struct T res;
  for (int i = 0; i < N; ++i)
    res.x[i] = __builtin_signbit (x.x[i]) ? -1 : 0;
  return res;
}
