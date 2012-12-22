// { dg-do compile }
// { dg-options "-std=gnu++11" }

typedef float __attribute__ ((vector_size (4 * sizeof (float)))) V4;
constexpr V4 v = { 1, 1, 1, 0 };
constexpr V4 r = v[0] + v; // { dg-bogus "not a constant expression" "" { xfail *-*-* } }
