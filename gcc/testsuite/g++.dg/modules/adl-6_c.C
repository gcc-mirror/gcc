// PR c++/117658
// { dg-additional-options "-fmodules -Wno-error=external-tu-local" }

import N;

namespace S {
  struct Z { template<typename T> operator T(); };
}

void test() {
  auto x = make();  // OK, decltype(x) is R::X in module M

  R::f(x);  // error: R and R::f are not visible here
  // { dg-error "" "" { target *-*-* } .-1 }

  f(x);  // OK, calls R::f from interface of M

  f(x, S::Z());  // error: S::f in module M not considered even though S is an associated namespace
  // { dg-error "" "" { target *-*-* } .-1 }

  apply_ok(x, S::Z());  // OK, S::f is visible in instantiation context

  apply_err(x);  // error: R::g has internal linkage and cannot be used outside N
  // { dg-message "here" "" { target *-*-* } .-1 }
  // { dg-warning "lookup of 'R::g'" "" { target *-*-* } 0 }
  // { dg-error "'g' was not declared" "" { target *-*-* } 0 }

  auto y = make_Y();
  f(y);  // OK, I::B::f and I::A::Y have matching innermost non-inline namespace
  g(y);  // OK, O::g is accessible through I::D::g

  f(y, 0);  // error: I::B::f(Y, int) is not attached to M
  // { dg-error "" "" { target *-*-* } .-1 }

  h(y);  // error: O::h is also not attached to M
  // { dg-error "" "" { target *-*-* } .-1 }
}
