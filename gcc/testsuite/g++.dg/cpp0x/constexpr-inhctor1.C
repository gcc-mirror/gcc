// PR c++/58511
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A(int, int = i) {}
  static const int i;
};

struct B : A
{
  using A::A;			// { dg-prune-output "A::i" }
};

constexpr B b(0);		// { dg-error "" }
