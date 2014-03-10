// PR c++/34057
// { dg-do compile { target c++11 } }

template <typename... T> struct A
{
  typedef T X __attribute__ ((vector_size (8))); // { dg-error "not expanded|T" }
};
