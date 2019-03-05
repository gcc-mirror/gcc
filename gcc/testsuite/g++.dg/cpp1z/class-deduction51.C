// PR c++/84937
// { dg-do compile { target c++17 } }

template<int, int> struct A {};

template<int I> struct B
{
  template<auto J> B(A<I,J>);
};

B b(A<0,0>{});
