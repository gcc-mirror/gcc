// PR target/85572
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2 -mavx2 -mno-avx512f" }
// { dg-final { scan-assembler-times {\mvpxor\M} 4 } }
// { dg-final { scan-assembler-times {\mvpcmpgtq\M} 2 } }
// { dg-final { scan-assembler-times {\mvpsubq\M} 2 } }

typedef long long V __attribute__((vector_size (16)));
typedef long long W __attribute__((vector_size (32)));

V
foo (V x)
{
  return x < 0 ? -x : x;
}

W
bar (W x)
{
  return x < 0 ? -x : x;
}
