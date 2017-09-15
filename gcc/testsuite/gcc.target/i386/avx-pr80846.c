/* PR target/80846 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx -mno-avx2" } */

typedef __int128 V __attribute__((vector_size (32)));
typedef long long W __attribute__((vector_size (32)));
typedef int X __attribute__((vector_size (16)));
typedef __int128 Y __attribute__((vector_size (64)));
typedef long long Z __attribute__((vector_size (64)));

W f1 (__int128 x, __int128 y) { return (W) ((V) { x, y }); }
__int128 f2 (W x) { return ((V)x)[0]; }
__int128 f3 (W x) { return ((V)x)[1]; }
W f4 (X x, X y) { union { X x; __int128 i; } u = { .x = x }, v = { .x = y }; return (W) ((V) { u.i, v.i }); }
X f5 (W x) { return (X)(((V)x)[0]); }
X f6 (W x) { return (X)(((V)x)[1]); }
W f7 (void) { return (W) ((V) { 2, 3 }); }
W f8 (X x) { union { X x; __int128 i; } u = { .x = x }; return (W) ((V) { u.i, 3 }); }
W f9 (X x) { union { X x; __int128 i; } u = { .x = x }; return (W) ((V) { 2, u.i }); }
W f10 (X x) { union { X x; __int128 i; } u = { .x = x }; return (W) ((V) { u.i, u.i }); }
#ifdef __AVX512F__
Z f11 (__int128 x, __int128 y, __int128 z, __int128 a) { return (Z) ((Y) { x, y, z, a }); }
__int128 f12 (Z x) { return ((Y)x)[0]; }
__int128 f13 (Z x) { return ((Y)x)[1]; }
__int128 f14 (Z x) { return ((Y)x)[2]; }
__int128 f15 (Z x) { return ((Y)x)[3]; }
Z f16 (X x, X y, X z, X a) { union { X x; __int128 i; } u = { .x = x }, v = { .x = y }, w = { .x = z }, t = { .x = a };
  return (Z) ((Y) { u.i, v.i, w.i, t.i }); }
X f17 (Z x) { return (X)(((Y)x)[0]); }
X f18 (Z x) { return (X)(((Y)x)[1]); }
X f19 (Z x) { return (X)(((Y)x)[2]); }
X f20 (Z x) { return (X)(((Y)x)[3]); }
Z f21 (void) { return (Z) ((Y) { 2, 3, 4, 5 }); }
Z f22 (X x) { union { X x; __int128 i; } u = { .x = x }; return (Z) ((Y) { u.i, 3, 4, 5 }); }
Z f23 (X x) { union { X x; __int128 i; } u = { .x = x }; return (Z) ((Y) { 2, u.i, 4, 5 }); }
Z f24 (X x) { union { X x; __int128 i; } u = { .x = x }; return (Z) ((Y) { 2, 3, u.i, 5 }); }
Z f25 (X x) { union { X x; __int128 i; } u = { .x = x }; return (Z) ((Y) { 2, 3, 4, u.i }); }
Z f26 (X x) { union { X x; __int128 i; } u = { .x = x }; return (Z) ((Y) { u.i, u.i, u.i, u.i }); }
#endif
