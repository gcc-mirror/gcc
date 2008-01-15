// { dg-options "-std=c++0x" }
template<int...> struct A
{
  void foo();
};

struct B
{
  template<int N> friend void A<N>::A::foo(); // { dg-error "declared as friend" }
};
