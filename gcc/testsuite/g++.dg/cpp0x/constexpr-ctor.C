// { dg-do compile { target c++11 } }

struct A
{
  int i;
  constexpr A() { }		// { dg-error "A::i" "" { target c++17_down } }
};
