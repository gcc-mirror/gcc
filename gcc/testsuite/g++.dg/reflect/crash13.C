// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

struct S {
  int j;
};

template <info R>
consteval int fn() {
  auto x = [:R:]; // { dg-error "cannot implicitly reference a class member" }
  return true;
}
static_assert (fn<^^S::j>());
