// { dg-options "-std=c++11" }
template<int...> struct A
{
  void foo();
};

struct B
{
  template<int N> friend void A<N>::A::foo(); // { dg-error "declared as friend" }
};
