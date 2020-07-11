// { dg-do compile { target c++20 } }

struct A {
  bool operator==(const A&) const;
};

struct D
{
  A i;
  bool operator==(const D& x) const = default; // { dg-error "A::operator==" }
  bool operator!=(const D& z) const = default; // { dg-error "D::operator==" }
};

constexpr D d{A()};
static_assert (d == d);		// { dg-error "non-constant|constexpr" }
static_assert (!(d != d));	// { dg-error "non-constant|constexpr" }
