// PR c++/34271
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<int> struct A
{
  static int i;
};

template<int N> int A<N>::i(decltype (A::i;	// { dg-error "expected primary-expression before" }
