// PR c++/51402

template<void> struct A   // { dg-error "not a valid type" }
{
  template<int,int> struct B {};
  template<int N> struct B<N,N> {};
};
