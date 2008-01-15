// PR c++/34399
template<int> struct X
{
  void foo();
};

struct Y {
  template<long N> friend void X<N>::X::foo(); // { dg-error "declared as friend" }
};
