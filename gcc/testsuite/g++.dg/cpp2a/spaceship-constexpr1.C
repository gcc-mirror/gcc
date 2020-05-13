// { dg-do compile { target c++20 } }

struct A
{
  int i;
  bool operator==(A a) const { return i == a.i; }
};

struct B
{
  A a;
  bool operator==(const B&) const = default; // { dg-error "A::operator==" }
};

constexpr bool x = B() == B();	// { dg-error "non-.constexpr" }
