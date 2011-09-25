// { dg-options -std=c++0x }

struct A
{
  int i;
  explicit constexpr A(int i): i(i) {}
};

struct B
{
  A a1 = 1;			// { dg-error "" }
  A a2 { 2 };
  A a3 = { 3 };			// { dg-error "" }
};

constexpr B b;			// { dg-error "B::B" }

// { dg-message "a1. is invalid" "" { target *-*-* } 11 }
