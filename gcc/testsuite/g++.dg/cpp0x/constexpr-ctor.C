// { dg-options -std=c++11 }

struct A
{
  int i;
  constexpr A() { }		// { dg-error "uninitialized member .A::i" }
};
