// PR c++/84937
// { dg-additional-options -std=c++17 }

template<int, int> struct A {};

template<int I> struct B
{
  template<auto J> B(A<I,J>);
};

B b(A<0,0>{});
