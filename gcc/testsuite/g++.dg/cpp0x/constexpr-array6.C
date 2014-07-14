// PR c++/58611
// { dg-do compile { target c++11 } }

struct A
{
  int i;
  constexpr A() {}		// { dg-error "A::i" }
};

struct B
{
  A a;
};

constexpr B b[] = { {} };	// { dg-error "A::A" }
