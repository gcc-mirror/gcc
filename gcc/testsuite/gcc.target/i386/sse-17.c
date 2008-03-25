/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
#include "sse2-check.h"
#include <xmmintrin.h>
extern void abort();
int untrue = 0;
typedef union {
  __v4sf v;
  float f[4];
} u;
void foo (u, u) __attribute__((noinline));
void foo (u a, u b) {
  if (b.f[0] != 7.0 || b.f[1] != 8.0 || b.f[2] != 3.0 || b.f[3] != 4.0)
    abort();
}
void bar (__v4sf, __v4sf) __attribute__((noinline));
void bar (__v4sf a __attribute((unused)), __v4sf b __attribute((unused))) { untrue = 0;}
__v4sf setupa () __attribute((noinline));
__v4sf setupa () { __v4sf t = { 1.0, 2.0, 3.0, 4.0 }; return t; }
__v4sf setupb () __attribute((noinline));
__v4sf setupb () { __v4sf t = { 5.0, 6.0, 7.0, 8.0 }; return t; }
void __attribute__((noinline))
sse2_test(void) {
  u a, b;
  a.v = setupa ();
  b.v = setupb ();
  if (untrue)
    bar(a.v, b.v);
  b.v = (__v4sf) _mm_movehl_ps ((__m128)a.v, (__m128)b.v);
  foo (a, b);
}
