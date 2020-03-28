// { dg-do compile { target c++2a } }

template<typename A, typename B>
  concept same_as = __is_same(A, B);

void f();

static_assert(requires { { f() } noexcept -> same_as<int>; });
// { dg-error "static assertion failed" "" { target *-*-* } .-1 }
// { dg-message "not .noexcept." "" { target *-*-* } .-2 }
// { dg-message "return-type-requirement" "" { target *-*-* } .-3 }
