/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler-times "pshufd" 3 } } */

typedef int v2si __attribute__((vector_size(8)));
typedef short v4hi __attribute__((vector_size(8)));
typedef char v8qi __attribute__((vector_size(8)));

v2si
foo (v2si a, v2si b)
{
    return __builtin_shufflevector (a, b, 1, 2);
}

v4hi
foo1 (v4hi a, v4hi b)
{
  return __builtin_shufflevector (a, b, 2, 3, 4, 5);
}

v8qi
foo2 (v8qi a, v8qi b)
{
  return __builtin_shufflevector (a, b, 4, 5, 6, 7, 8, 9, 10, 11);
}
