// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [meta.reflection.access.queries].

#include <meta>

using namespace std::meta;

consteval access_context fn() {
  return access_context::current();
}

class Cls {
  int mem;
  friend consteval access_context fn();
public:
  static constexpr auto r = ^^mem;
};

static_assert(is_accessible(Cls::r, fn()));                             // OK
static_assert(!is_accessible(Cls::r, access_context::current()));       // OK
static_assert(is_accessible(Cls::r, access_context::unchecked()));      // OK
