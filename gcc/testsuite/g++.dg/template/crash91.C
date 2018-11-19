// PR c++/28293

template<int> void foo();

struct A
{
  typedef void foo<0>(); // { dg-error "16:explicit template argument list not allowed" } 
};
