// { dg-do compile { target c++11 } }
// Origin: Benjamin Kosnik <bkoz at gcc dot gnu dot org>
// PR c++/17743: Attributes applied to typedefs.

struct A {
  typedef char layout_type[sizeof(double)]
  [[gnu::aligned(alignof(double)]]); // { dg-error "expected" }
  layout_type data;
};

struct B {
  typedef char layout_type[sizeof(double)];
  layout_type data  [[gnu::aligned(alignof(double))]];
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<alignof(A) == alignof(B)> a1;// { dg-error "incomplete type and cannot be defined" }
