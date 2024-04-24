/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1" } */
/* { dg-final { scan-assembler-not "%zmm" } } */

typedef float __m512 __attribute__ ((__vector_size__ (64), __may_alias__));

void __attribute__((target("avx10.1-256"))) callee256(__m512 *a, __m512 *b) { *a = *b; }
