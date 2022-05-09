/* { dg-do compile } */
/* { dg-options "-msse4.1 -O2" } */
/* { dg-final { scan-assembler-times {(?n)pmovzxbq[ \t]+} "4" } } */
/* { dg-final { scan-assembler-not {(?n)pinsrw[ \t]+} } } */

#include<immintrin.h>

__m128i foo (void *p){
  return _mm_cvtepu8_epi64(_mm_loadu_si16(p));
}

__m128i foo2 (short a){
  return _mm_cvtepu8_epi64(_mm_set_epi16(0, 0, 0, 0, 0, 0, 0, a));
}

__m128i
foo3 (void *p){
  return _mm_cvtepu8_epi64((__m128i)__extension__(__m128h) {*(_Float16 const*)p, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f});
}

__m128i
foo4 (_Float16 a){
  return _mm_cvtepu8_epi64((__m128i)__extension__(__m128h) {a, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f});
}
