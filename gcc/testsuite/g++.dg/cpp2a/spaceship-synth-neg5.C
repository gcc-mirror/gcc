// { dg-do compile { target c++20 } }
// { dg-options "" }

#include <compare>

struct C {
  int y;
  int x[];					// { dg-message "cannot default compare flexible array member" }
  auto operator<=>(C const&) const = default;	// { dg-message "is implicitly deleted because the default definition would be ill-formed" }
};

bool
foo (C &c1, C &c2)
{
  return c1 > c2;	// { dg-error "use of deleted function" }
}
