// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::define_static_array.

#include <meta>

struct A {
  int a;
  consteval A (int x) : a(x) {}
  consteval A (const A &x) : a(x.a + 1) {}
};
static_assert (std::vector <A> { 1 }[0].a == 2);
constexpr auto a = std::define_static_array (std::vector <A> { 1, 11, 21 });
static_assert (a.size () == 3);
static_assert (a[0].a >= 2 && a[1].a >= 12 && a[2].a >= 22);
// TODO: I believe there should be at least 2 copy ctors per element rather
// than just one.  One is already in the vector construction.
// Then [meta.define.static]/10 models it as if reflect_constant was called
// on each element and reflect_constant takes the class by value rather than
// reference, so one needs to copy construct from a const reference to the
// range element into a TARGET_EXPR of the reflect_constant argument.
// And not sure if there shouldn't be further bumps inside of
// reflect_constant, libcxx bumps 3 times.
// static_assert (a[0].a == 3 && a[1].a == 13 && a[2].a == 23);

// I believe the copy-constructor of this class breaks the rule for NTTP
// from [temp.arg.nontype] p4 (copies are not structurally equivalent),
// that we seem to not currently implement. It makes this test moot
// as reflect_constant should throw an exception.
// So language seems to say, that you should have no way to observe this copies.
