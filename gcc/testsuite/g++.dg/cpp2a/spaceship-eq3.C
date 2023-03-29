// { dg-do compile { target c++20 } }

struct A {
  bool operator==(const A&) const;
};

struct D
{
  A i;
  bool operator==(const D& x) const = default; // { dg-error "A::operator==" "" { target c++20_down } }
  bool operator!=(const D& z) const = default; // { dg-error "D::operator==" "" { target c++20_down } }
// { dg-error "called" "" { target { c++23 && implicit_constexpr } } .-1 }
};

constexpr D d{A()};
static_assert (d == d);		// { dg-error "constant|constexpr" }
static_assert (!(d != d));	// { dg-error "constant|constexpr" }
