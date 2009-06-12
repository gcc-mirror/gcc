/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mavx" } */

typedef long long __m256i __attribute__ ((__vector_size__ (32), __may_alias__));


__m256i
bar (__m256i x)
{
  return x;
}

/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-32,\[^\\n\]*sp" } } */
