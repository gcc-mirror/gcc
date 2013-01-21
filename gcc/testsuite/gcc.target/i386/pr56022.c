/* { dg-do compile } */
/* { dg-options "-mavx" } */

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));
__attribute__((target("no-avx"))) static int currentImplementationSupported()
{}
__m256 foo0(__m256 a) {}
