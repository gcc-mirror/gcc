// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test invoking functions with a reflection argument.

using info = decltype(^^int);

consteval void foo (info) { }
constexpr void bar (info) { } // { dg-error "function of consteval-only type must be declared .consteval." }
void baz (info) { }  // { dg-error "function of consteval-only type must be declared .consteval." }

void
f ()
{
  foo (^^void);
  bar (^^void);  // { dg-error "consteval-only expressions" }
  baz (^^void);  // { dg-error "consteval-only expressions" }
}

constexpr void
g ()
{
  foo (^^void);
  bar (^^void);  // { dg-error "consteval-only expressions" }
  baz (^^void);  // { dg-error "consteval-only expressions" }
}

consteval void
h ()
{
  foo (^^void);
  bar (^^void);
  baz (^^void);
}
