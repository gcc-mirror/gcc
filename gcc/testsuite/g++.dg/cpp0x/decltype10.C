// PR c++/34271
// { dg-do compile { target c++11 } }

template<int> struct A
{
  static int i;
};

template<int N> int A<N>::i(decltype (A::i;	// { dg-error "expected" }
