// PR c++/118387
// { dg-do compile { target c++20 } }

#include <compare>

struct B {};

struct A
{
  B b;			// { dg-error "no match for 'operator<=>' in '\[^\n\r]*' \\\(operand types are 'B' and 'B'\\\)" }
  int operator<=> (const A &) const = default;	// { dg-error "invalid 'static_cast' from type 'const std::strong_ordering' to type 'int'" }
};

int
main ()
{
  A a;
  return a <=> a;	// { dg-error "use of deleted function 'constexpr int A::operator<=>\\\(const A&\\\) const'" }
}
