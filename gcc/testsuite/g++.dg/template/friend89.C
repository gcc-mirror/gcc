// PR c++/39057

struct A
{
  template<int> void foo();
};

template<int> struct B
{
  friend void A::foo<0>(int = 0); // { dg-error "friend declaration" }
};
