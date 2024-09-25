// PR c++/105593
// { dg-do compile { target c++14 } }
// { dg-options "-mavx512fp16 -W -Wall -O2" }
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <x86intrin.h>

auto f1 () { return _mm_undefined_pd (); }
auto f2 () { return _mm_undefined_ps (); }
auto f3 () { return _mm_undefined_si128 (); }
auto f4 () { return _mm_undefined_ph (); }
auto f5 () { return _mm256_undefined_pd (); }
auto f6 () { return _mm256_undefined_ps (); }
auto f7 () { return _mm256_undefined_si256 (); }
auto f8 () { return _mm256_undefined_ph (); }
auto f9 () { return _mm512_undefined_pd (); }
auto f10 () { return _mm512_undefined_ps (); }
auto f11 () { return _mm512_undefined_epi32 (); }
auto f12 () { return _mm512_undefined_ph (); }

// { dg-bogus "is used uninitialized" "" { target *-*-* } 0 }
