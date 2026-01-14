// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on types.  Invalid cases.

struct A;

void
f1 ()
{
  const auto r = ^^double; // { dg-error "consteval-only variable" }
  constexpr auto r2 = ^^int;
  r2; // { dg-error "consteval-only expressions" }
  ^^void; // { dg-error "consteval-only expressions" }
  ^^int == ^^int; // { dg-error "consteval-only expressions" }
  (void) ^^float; // { dg-error "consteval-only expressions" }
  auto rr = r; // { dg-error "consteval-only variable" }

  constexpr auto x = &(^^int); // { dg-error "lvalue required" }

  // These are verboten.
  constexpr auto a = ^^auto;  // { dg-error "cannot be applied to" }
  constexpr auto d = ^^decltype(auto);  // { dg-error "cannot be applied to" }

  constexpr auto r3 = ^^A;
  [: r3 :] inc;	// { dg-error "expected a reflection of an expression" }
}

constexpr void
f2 ()
{
  auto r = ^^int; // { dg-error "consteval-only variable" }
}

void
f3 ()
{
  [: ^^:: :] i; // { dg-error "expected" }
  typename [: ^^:: :] i2;  // { dg-error "expected" }
}

void
f4 ()
{
  using T1 = ^^int;  // { dg-error "expected type-specifier" }
}

template<typename... T>
void
f5 ()
{
  constexpr auto r = ^^T; // { dg-error "parameter packs not expanded with" }
}

void
g ()
{
  f2 ();
}
