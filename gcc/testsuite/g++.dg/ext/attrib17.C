// { dg-do compile }
// Origin: Benjamin Kosnik <bkoz at gcc dot gnu dot org>
// PR c++/17743: Attributes applied to typedefs.

struct A {
  typedef char layout_type[sizeof(double)]
  __attribute__((aligned(__alignof__(double))));
  layout_type data;
};

struct B {
  typedef char layout_type[sizeof(double)];
  layout_type data   __attribute__((aligned(__alignof__(double))));
};

template<bool> struct StaticAssert;
template<> struct StaticAssert<true> {};

StaticAssert<__alignof__(A) == __alignof__(B)> a1;
