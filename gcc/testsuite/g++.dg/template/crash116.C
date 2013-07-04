// PR c++/38634

template<int> struct A
{
  A();
};

template<int N, char> A<N>::A()  // { dg-error "template|required" }
{
  struct B {};
}

A<0> a;
