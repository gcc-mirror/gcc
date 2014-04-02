// PR c++/34058
// { dg-do compile { target c++11 } }

template <typename...T> struct A
{
  typedef T X;	// { dg-error "not expanded|T" }
};

A<int> a;
