// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test invoking functions with a reflection argument.

using info = decltype(^^int);

consteval void foo (info) { }
constexpr void bar (info) { }
void baz (info) { }

void
f ()
{
  foo (^^void);
  bar (^^void);  // { dg-error "consteval-only value" }
  baz (^^void);  // { dg-error "consteval-only value" }
}

constexpr void
g ()
{
  foo (^^void);
  bar (^^void);  // { dg-error "consteval-only value" }
  baz (^^void);  // { dg-error "consteval-only value" }
}

consteval void
h ()
{
  foo (^^void);
  bar (^^void);
  baz (^^void);
}
