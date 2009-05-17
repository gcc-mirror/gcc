// PR c++/40139

template<int> struct A
{
  static int i;
};

template<int N> int A<N>::i = { A::~A }; // { dg-error "" }

template class A<0>;
