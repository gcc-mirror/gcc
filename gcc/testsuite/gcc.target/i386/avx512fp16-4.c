/* { dg-do assemble { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef _Float16 __m256h __attribute__ ((__vector_size__ (32), __may_alias__));
typedef _Float16 __m512h __attribute__ ((__vector_size__ (64), __may_alias__));
typedef _Float16 __m128h __attribute__ ((__vector_size__ (16), __may_alias__));

extern __m128h x128, y128;
extern __m256h x256, y256;
extern __m512h x512, y512;

__m128h
foo1 (float f1, __m128h f2)
{
  x128 = y128;
  return f2;
}

__m256h
foo2 (float f1, __m256h f2)
{
  x256 = y256;
  return f2;
}

__m512h
foo3 (float f1, __m512h f2)
{
  x512 = y512;
  return f2;
}
