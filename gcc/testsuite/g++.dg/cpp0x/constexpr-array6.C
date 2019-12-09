// PR c++/58611
// { dg-do compile { target c++11 } }

struct A
{
  int i;
  constexpr A() {}		// { dg-error "A::i" "" { target c++17_down } }
};

struct B
{
  A a;
};

constexpr B b[] = { {} };	// { dg-error "A::A" "" { target c++17_down } }
// { dg-error "is not a constant expression" "" { target c++2a } .-1 }
