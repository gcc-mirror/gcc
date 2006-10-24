// PR c++/29020

template<int> struct A
{
  void foo();
};

struct B
{
  template<int N> friend void A<N>::A::foo();
};

