// PR c++/94252
// { dg-do compile { target c++2a } }

auto f = []{ return 0; };
static_assert(requires { f(); });
static_assert(requires { requires requires { f(); }; });

template<typename A, typename B>
  concept same_as = __is_same(A, B);

struct S { int f(int) noexcept; };
static_assert(requires(S o, int i) {
  o.f(i);
  { o.f(i) } noexcept -> same_as<int>;
});

template<typename T>
  concept c = requires (T t) { requires (T)5; }; // { dg-error "has type .int." }
// { dg-bogus "not satisfied" "" { target *-*-* } .-1 }

int
foo()
{
  requires { requires c<int>; };
  requires { { 5 } -> same_as<bool>; };
  requires { requires !requires { { 5 } -> same_as<bool>; }; };
  return requires { requires 5; }; // { dg-error "has type .int." }
}
