// PR c++/92812
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wno-psabi" }
// Paren-init in a member init list with vector types.

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

__m128 m;
__m128 g(m);
__m128 ag[](m, m, m);
__m128 ag2[]({}, {}, {});

struct A {
  __m128 a1;
  __m128 a2[2];
  __m128 a3[2];
  A() : a1(m),
	a2(m, m),
	a3({}, m)
    { }
};
