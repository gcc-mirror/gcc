// P2002: deleted if a subobject <=> has non-category type.
// { dg-do compile { target c++20 } }

#include <compare>

struct A
{
  bool operator<=>(const A&) const;
};

struct B
{
  A a;				// { dg-message "bool" }
  auto operator<=>(const B&) const = default;
};

int main()
{
  auto x = B() <=> B();		// { dg-error "deleted" }
}
