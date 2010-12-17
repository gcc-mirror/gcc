// { dg-options -std=c++0x }

struct A
{
  int i;
  constexpr A() { }		// { dg-error "uninitialized member .A::i" }
};
