// PR c++/47301
// { dg-options "-std=c++11 -fabi-version=1" }

struct A
{
  constexpr operator int ()
  {
    return 1;
  }
};

template < int > struct B
{
  static constexpr A a = A();
  int ar[a];
};
