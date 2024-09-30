/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-not "vzeroupper" { target { ! ia32 } } } } */

#include <immintrin.h>

struct B {
  union {
    __m512 f;
    __m512i s;
  };
};

struct B foo(int n) {
  struct B res;
  res.s = _mm512_set1_epi32(n);

  return res;
}

__m512i bar(int n) {
  struct B res;
  res.s = _mm512_set1_epi32(n);

  return res.s;
}
