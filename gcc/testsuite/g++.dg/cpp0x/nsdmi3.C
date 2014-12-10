// { dg-do compile { target c++11 } }

struct A
{
  int i;
  explicit constexpr A(int i): i(i) {}
};

struct B
{
  A a1 = 1;			// { dg-error "" }
  A a2 { 2 };
  A a3 = { 3 };			// { dg-error "explicit" }
};

constexpr B b;			// { dg-error "B::B" }

// { dg-prune-output "B::a1" }
