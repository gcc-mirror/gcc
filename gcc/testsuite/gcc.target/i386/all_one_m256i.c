/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef long long __m256i __attribute__ ((__vector_size__ (32)));
typedef int __v8si __attribute__ ((__vector_size__ (32)));

__m256i foo ()
{
  __m256i minus_1 = (__m256i) (__v8si) { -1, -1, -1, -1, -1, -1, -1, -1 };

  return minus_1;
}

/* { dg-final { scan-assembler "vpcmpeqd" } } */
