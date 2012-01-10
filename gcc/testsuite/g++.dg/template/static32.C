// PR c++/51430
// { dg-do compile }

template<int> struct A
{
  static const int x[] = 0;	// { dg-error "in-class initialization|initializer fails" }
};

A<0> a;
