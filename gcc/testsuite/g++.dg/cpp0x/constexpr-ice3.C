// PR c++/46289
// { dg-do compile { target c++11 } }

struct A
{
  int i;
};

struct B
{
  A a;
  constexpr B(): a({1,2}) { }	// { dg-error "" }
};
