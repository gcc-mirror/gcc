// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.ref].

#include <meta>

struct B {
  int b;
};
struct C : B {
  constexpr int get() const { return b; }
};
struct D : B, C { }; // { dg-warning "inaccessible" }

constexpr int f() {
  D d = {1, {}};

  // b unambiguously refers to the direct base class of type B,
  // not the indirect base class of type B
  B& b = d.[: std::meta::bases_of(^^D, std::meta::access_context::current())[0] :];
  b.b += 10;
  return 10 * b.b + d.get();
}
static_assert(f() == 110);
