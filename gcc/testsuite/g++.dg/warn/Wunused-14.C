// Test that -Wunused should not emit a warning on the initialization of
// non-POD arrays. See PR c++/39803.
// { dg-do compile }
// { dg-options "-Wunused" }

#include <utility>

using std::pair;

int foo() {
  pair<int, const char*> components[3]; // { dg-bogus "value computed is not used" }
  components[0].first = 0;
  return 0;
}
