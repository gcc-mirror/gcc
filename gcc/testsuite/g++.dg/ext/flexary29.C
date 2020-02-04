// PR c++/84578
// { dg-do compile { target c++11 } }
// { dg-options -Wno-pedantic }

struct A
{
  constexpr A() : i(), x() {}	// { dg-error "flexible" }
  int i;
  char x[];
};

A a;
