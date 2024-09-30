// PR middle-end/70887
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && c++11 } } }
// { dg-options "-O2 -msse2" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <x86intrin.h>

enum R { S };
template <R> struct C { static constexpr int value = 10; };
template <typename R, template <R> class T, R... r>
struct A {
  template <int, R...> struct B;
  template <int N, R M, R... O>
  struct B<N, M, O...> {
    static constexpr int d = T<M>::value;
    static __m128i generate()
    {
      __attribute__((__vector_size__(16))) long long
      a = generate(),
      b = _mm_bslli_si128 (a, 1),
      c = _mm_bsrli_si128 (_mm_set1_epi32(d), 12);
      return _mm_or_si128 (b, c);
    }
  };
  A () { B<0, r...>::generate(); }
};

int
main () {
  using RI = A<R, C, S>;
  RI ri;
}
