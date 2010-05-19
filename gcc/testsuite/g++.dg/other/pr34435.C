/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-msse2" } */
/* { dg-require-effective-target sse2 } */

#include <emmintrin.h>

class Vec {
    __m128i vec;
public:
    Vec(int mm) {
        vec = _mm_set1_epi16(mm);
    }
  operator __m128i() const {
      return vec;
    }
};

int main() {
  _mm_shuffle_epi32(Vec(5), _MM_SHUFFLE(3,3,3,3));
}
