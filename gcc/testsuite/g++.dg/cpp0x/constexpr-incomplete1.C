// { dg-options -std=c++0x }

struct A
{
  static constexpr A a = 1;	// { dg-error "incomplete" }
  constexpr A(int i) { }
};
