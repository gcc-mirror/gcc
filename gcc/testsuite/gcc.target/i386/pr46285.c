/* { dg-do compile } */
/* { dg-options "-mavx -fsplit-stack -mtune=generic" } */
/* { dg-require-effective-target split_stack } */

typedef char __m256 __attribute__ ((__vector_size__ (32)));
void foo (__m256 x) {}
