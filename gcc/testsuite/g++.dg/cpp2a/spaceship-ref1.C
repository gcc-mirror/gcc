// Reject &&.
// { dg-do compile { target c++20 } }

struct A
{
  bool operator==(const A&) const && = default; // { dg-error "ref-qualifier" }
};

struct B
{
  friend bool operator==(const B&&, const B&&) = default; // { dg-error "" }
};
