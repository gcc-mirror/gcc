// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [meta.reflection.access.context].

#include <meta>

using namespace std::meta;

struct A {
  int a = 0;
  consteval A(int p) : a(p) {}
};
struct B : A {
  using A::A;
  consteval B(int p, int q) : A(p * q) {}
  info s = access_context::current().scope();
};
struct C : B { using B::B; };

struct Agg {
  consteval bool eq(info rhs = access_context::current().scope()) {
    return s == rhs;
  }
  info s = access_context::current().scope();
};

namespace NS {
  static_assert(Agg{}.s == access_context::current().scope());              // OK
  static_assert(Agg{}.eq());                                                // OK
  static_assert(B(1).s == ^^B);                                             // OK
  static_assert(is_constructor(B{1, 2}.s) && parent_of(B{1, 2}.s) == ^^B);  // OK
  static_assert(is_constructor(C{1, 2}.s) && parent_of(C{1, 2}.s) == ^^B);  // OK

  auto fn() -> [:is_namespace(access_context::current().scope()) ? ^^int : ^^bool:];
  static_assert(return_type_of(type_of(^^fn)) == ^^int);
  static_assert(type_of(^^fn) == ^^auto()->int);                            // OK

  template<auto R>
    struct TCls {
      consteval bool fn()
        requires (is_type(access_context::current().scope())) {
          return true;                  // OK, scope is TCls<R>.
        }
    };
  static_assert(TCls<0>{}.fn());        // OK
}
