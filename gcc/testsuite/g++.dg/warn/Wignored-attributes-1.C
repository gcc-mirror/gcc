// PR c++/69884
// { dg-do compile }

typedef float __m128 __attribute__((__vector_size__(16), __may_alias__));
template <typename> struct A;
template <> struct A<__m128>; // { dg-warning "ignoring attributes on template argument" }
