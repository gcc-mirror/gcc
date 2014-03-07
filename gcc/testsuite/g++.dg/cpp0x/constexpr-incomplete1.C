// { dg-do compile { target c++11 } }

struct A
{
  static constexpr A a = 1;	// { dg-error "incomplete" }
  constexpr A(int i) { }
};
