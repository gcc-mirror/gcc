/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mno-avx512vl -O2" } */
/* { dg-final { scan-assembler-times {(?n)vpblendvb[ \t]+%xmm[0-9]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vpblendvb[ \t]+%ymm[0-9]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)(?:vpbroadcastw|vmovdq[ua]16)[ \t].*%zmm[0-9].*%k[0-7]} 1 } } */

typedef _Float16 v32hf __attribute__((vector_size(64)));
typedef _Float16 v16hf __attribute__((vector_size(32)));
typedef _Float16 v8hf __attribute__((vector_size(16)));

v8hf
foo1 (v8hf a, _Float16 b, int c)
{
  a[c] = b;
  return a;
}

v16hf
foo2 (v16hf a, _Float16 b, int c)
{
  a[c] = b;
  return a;
}

v32hf
foo3 (v32hf a, _Float16 b, int c)
{
  a[c] = b;
  return a;
}
