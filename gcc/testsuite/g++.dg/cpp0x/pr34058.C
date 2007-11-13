// PR c++/34058
// { dg-do compile }
// { dg-options "-std=c++0x" }

template <typename...T> struct A
{
  typedef T X;	// { dg-error "not expanded|T" }
};

A<int> a;
