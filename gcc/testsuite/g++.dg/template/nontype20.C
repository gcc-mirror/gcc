// PR c++/48003
// { dg-options "-fpermissive -w" }
// Test that we allow integer overflow in constant exprs with -fpermissive

template<int N>
struct test
{
  typedef test<N - 1> prior;
};

test<-2147483647-1> f;
