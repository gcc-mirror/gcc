// PR c++/46289
// { dg-options -std=c++0x }

struct A
{
  int i;
};

struct B
{
  A a;
  constexpr B(): a({1,2}) { }	// { dg-error "" }
};
