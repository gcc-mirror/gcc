/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -mno-sse3" } */
/* { dg-final { scan-assembler-times "shufps" 15 } } */
/* { dg-final { scan-assembler-times "pshufd" 2 } } */

typedef int v4si __attribute__((vector_size(16)));

v4si
__attribute__((noipa))
foo (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 1, 2, 5, 3);
}

v4si
__attribute__((noipa))
foo1 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 1, 5, 2, 3);
}

v4si
__attribute__((noipa))
foo2 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 1, 2, 3, 5);
}

v4si
__attribute__((noipa))
foo3 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 1, 4, 5, 6);
}

v4si
__attribute__((noipa))
foo4 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 3, 6, 7, 5);
}

v4si
__attribute__((noipa))
foo5 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 2, 4, 7, 6);
}

v4si
__attribute__((noipa))
foo6 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 2, 4, 3, 6);
}

v4si
__attribute__((noipa))
foo7 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 2, 3, 4, 6);
}

v4si
__attribute__((noipa))
foo8 (v4si a, v4si b)
{
  return __builtin_shufflevector (a, b, 2, 4, 6, 3);
}

