// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Detect taking the reflection of a using-declarator as per [expr.reflect]/5.

namespace NS {
  namespace Inner {
    consteval int fn() { return 42; }
    template <auto V> consteval int tfn() { return V; }
  }  // namespace Inner

  using Inner::fn;
  using Inner::tfn;
}  // namespace NS

// splice-expressions
static_assert([:^^NS::fn:]() == 42);  // { dg-error "cannot be applied to a using-declaration" }
static_assert(template [:^^NS::tfn:]<4>() == 4);  // { dg-error "cannot be applied to a using-declaration|expected" }

// nested proxies
struct A { int m; };
struct B : A { using A::m; };
struct C : B { using B::m; };

static_assert(&[:^^C::m:] == &A::m); // { dg-error "cannot be applied to a using-declaration" }
