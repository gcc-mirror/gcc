// { dg-do compile { target c++17 } }
// { dg-options "-Wdangling-reference" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// Check that we warn here even without -Wsystem-headers.

#include <optional>
#include <string>

auto f() -> std::optional<std::string>;

void
g ()
{
  for (char c : f().value()) { (void) c; } // { dg-warning "dangling reference" }
}
