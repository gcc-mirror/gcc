// PR c++/47301
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=1" }

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
