/* { dg-do compile } */
/* { dg-options "-O0 -msse2" } */

#include <xmmintrin.h>

__m128d foo1(__m128d z, __m128d  a, int N) { 
  int i;
  for (i=0; i<N; i++) {
    a = _mm_add_ps(z, a); /* { dg-error "incompatible type" } */
  }
  return a;
}
/* { dg-message "note: expected '.*'.* but argument is of type '.*'" "note: expected" { target *-*-* } 0 } */
