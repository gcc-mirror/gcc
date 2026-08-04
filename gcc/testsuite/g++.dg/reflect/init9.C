// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test returning consteval-only exprs.

using info = decltype(^^int);

info foo ();
constexpr info bar ();
void baz (info);

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
bad1 ()
{
  return ^^int;  // { dg-error "consteval-only value" }
}

info
bad2 ()
{
  return ^^int;  // { dg-error "consteval-only value" }
}

constexpr auto
bad3 (info i)
{
  return i;
}

template<info R>
info
bad4 ()
{
  return R;
}
