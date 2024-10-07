/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bf16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpermi2w" 3 } } */

typedef __bf16 v8bf __attribute__((vector_size(16)));
typedef __bf16 v16bf __attribute__((vector_size(32)));
typedef __bf16 v32bf __attribute__((vector_size(64)));

v8bf foo0(v8bf a, v8bf b)
{
  return __builtin_shufflevector(a, b, 1, 3, 5, 7, 9, 11, 13, 15);
}

v16bf foo1(v16bf a, v16bf b)
{
  return __builtin_shufflevector(a, b, 1, 3, 5, 7, 9, 11, 13, 15,
                                 17, 19, 21, 23, 25, 27, 29, 31);
}

v32bf foo2(v32bf a, v32bf b)
{
  return __builtin_shufflevector(a, b, 1, 3, 5, 7, 9, 11, 13, 15, 
                                 17, 19, 21, 23, 25, 27, 29, 31, 
                                 33, 35, 37, 39, 41, 43, 45, 47, 
                                 49, 51, 53, 55, 57, 59, 61, 63);
}
