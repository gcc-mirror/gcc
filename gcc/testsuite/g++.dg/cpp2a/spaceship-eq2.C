// { dg-do compile { target c++2a } }

struct D
{
  int i;
  bool operator==(const D& x) const = default; // OK, returns x.i == y.i
  bool operator!=(const D& z) const = default;  // OK, returns !(*this == z)
};

constexpr D d{42};
static_assert (d == d);
static_assert (!(d != d));
