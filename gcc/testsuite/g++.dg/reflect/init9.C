// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test returning consteval-only exprs.

using info = decltype(^^int);

info foo (); // { dg-error "function of consteval-only type must be declared .consteval." }
constexpr info bar (); // { dg-error "function of consteval-only type must be declared .consteval." }
void baz (info); // { dg-error "function of consteval-only type must be declared .consteval." }

consteval info
ok1 ()
{
  return ^^int;
}

consteval info
ok2 ()
{
  constexpr info r = ^^int;
  return r;
}

consteval auto
ok3 (info i)
{
  return i;
}

constexpr info
bad1 () // { dg-error "function of consteval-only type must be declared .consteval." }
{
  return ^^int;  // { dg-error "consteval-only expressions" }
}

info
bad2 () // { dg-error "function of consteval-only type must be declared .consteval." }
{
  return ^^int;  // { dg-error "consteval-only expressions" }
}

constexpr auto
bad3 (info i) // { dg-error "function of consteval-only type must be declared .consteval." }
{
  return i;  // { dg-error "consteval-only expressions" }
}

template<info R>
info
bad4 () // { dg-error "function of consteval-only type must be declared .consteval." }
{
  return R;  // { dg-error "consteval-only expressions" }
}
