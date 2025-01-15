// PR c++/118387
// { dg-do compile { target c++20 } }

#include <compare>

struct A {
  int operator<=> (const A &) const;
};

struct B {
  A a;
  int operator<=> (const B &) const = default;	// { dg-message "'constexpr int B::operator<=>\\\(const B&\\\) const' is implicitly deleted because the default definition would be ill-formed:" }
};			// { dg-error "invalid 'static_cast' from type 'const std::strong_ordering' to type 'int'" "" { target *-*-* } .-1 }

struct C {
  int operator<=> (const C &) const = default;	// { dg-message "'constexpr int C::operator<=>\\\(const C&\\\) const' is implicitly deleted because the default definition would be ill-formed:" }
};			// { dg-error "invalid 'static_cast' from type 'const std::strong_ordering' to type 'int'" "" { target *-*-* } .-1 }

struct D {
  auto operator<=> (const D &) const = default;
};

struct E {
  D a;			// { dg-error "three-way comparison of 'E::a' has type 'std::strong_ordering', which does not convert to 'int'" }
  int operator<=> (const E &) const = default;	// { dg-message "'constexpr int E::operator<=>\\\(const E&\\\) const' is implicitly deleted because the default definition would be ill-formed:" }
};			// { dg-error "invalid 'static_cast' from type 'const std::strong_ordering' to type 'int'" "" { target *-*-* } .-1 }

struct F {
  A a;
  int operator<=> (const F &) const = default;
};

struct G {
  int operator<=> (const G &) const = default;
};

struct H {
  D a;
  int operator<=> (const H &) const = default;
};

auto
foo (B a, B b)
{
  return a <=> b;	// { dg-error "use of deleted function 'constexpr int B::operator<=>\\\(const B&\\\) const'" }
}

auto
bar (C a, C b)
{
  return a <=> b;	// { dg-error "use of deleted function 'constexpr int C::operator<=>\\\(const C&\\\) const'" }
}

auto
baz (E a, E b)
{
  return a <=> b;	// { dg-error "use of deleted function 'constexpr int E::operator<=>\\\(const E&\\\) const'" }
}
