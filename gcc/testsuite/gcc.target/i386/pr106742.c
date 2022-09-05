/* { dg-do compile } */
/* { dg-options "-msse2 -mno-sse4 -O1" } */
typedef __bf16 v8bf __attribute__ ((__vector_size__ (16)));

v8bf
vec_init_dup_v8bf (__bf16 a1)
{
  return __extension__ (v8bf) { a1, a1, a1, a1, a1, a1, a1, a1 };
}
/* { dg-final { scan-assembler-times "pinsrw" 1} } */
