// PR target/85572
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2 -msse2 -mno-sse3" }
// { dg-final { scan-assembler-times {\mpsrad\M} 1 } }
// { dg-final { scan-assembler-times {\mpshufd\M} 1 } }
// { dg-final { scan-assembler-times {\mpxor\M} 1 } }
// { dg-final { scan-assembler-times {\mpsubq\M} 1 } }

typedef long long V __attribute__((vector_size (16)));

V
foo (V x)
{
  return x < 0 ? -x : x;
}
