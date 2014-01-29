// PR c++/58674
// { dg-do compile { target c++11 } }

template<int> struct A {};

template<int N> using B = A<N>;

template<typename T> struct C
{
  B<T::i> b;  // { dg-error "not usable" }
};

struct X
{
  static const int i;
};

C<X> c;
