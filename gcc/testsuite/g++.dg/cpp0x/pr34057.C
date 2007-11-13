// PR c++/34057
// { dg-do compile }
// { dg-options "-std=c++0x" }

template <typename... T> struct A
{
  typedef T X __attribute__ ((vector_size (8))); // { dg-error "not expanded|T" }
};
