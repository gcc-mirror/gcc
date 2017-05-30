// PR rtl-optimization/80385
// { dg-do compile { target { i?86-*-* x86_64-*-* } } }
// { dg-options "-Ofast -msse2" }

#include <x86intrin.h>

__m128 a, e;
struct A { __m128 b; A (); A (__m128 x) : b(x) {} };
A operator+ (A, A);
A operator- (A) { __m128 c = -a; return c; }
A foo (A x) { __m128 d = x.b; return _mm_andnot_ps (d, e); }
struct B { A n[1]; };
void bar (B x) { A f = foo (x.n[0]); A g = f + A (); }
void baz () { B h; B i; A j; i.n[0] = -j; h = i; B k = h; bar (k); }
