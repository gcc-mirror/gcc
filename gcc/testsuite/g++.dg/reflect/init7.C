// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we properly detect using consteval-only variables without
// constexpr/constinit.

using info = decltype(^^int);

info r1 = ^^int;  // { dg-error ".r1. is initialized with a consteval-only value but is not declared .constexpr." }
const info r2 = ^^int;  // { dg-error ".r2. is initialized with a consteval-only value but is not declared .constexpr." }

constexpr info r3 = ^^int;
constinit info r4 = ^^int;  // { dg-error ".r4. is initialized with a consteval-only value but is not declared .constexpr." }
const info *const p1 = &r3;  // { dg-error ".p1. is initialized with a consteval-only value but is not declared .constexpr." }
info *p2;
const info &q = r3;  // { dg-error ".q. is initialized with a consteval-only value but is not declared .constexpr." }

void
g ()
{
  info l1 = ^^int;  // { dg-error ".l1. is initialized with a consteval-only value but is not declared .constexpr." }
  const info l2 = ^^int;  // { dg-error ".l2. is initialized with a consteval-only value but is not declared .constexpr." }
  constexpr info l3 = ^^int;
  static info l4 = ^^int;  // { dg-error ".l4. is initialized with a consteval-only value but is not declared .constexpr." }
  static const info l5 = ^^int;  // { dg-error ".l5. is initialized with a consteval-only value but is not declared .constexpr." }
  static constexpr info l6 = ^^int;
  static constinit info l7 = ^^int;  // { dg-error ".l7. is initialized with a consteval-only value but is not declared .constexpr." }
}

consteval void
f ()
{
  info l1 = ^^int;
  const info l2 = ^^int;
  constexpr info l3 = ^^int;
  // Are these really OK?  Only if we don't call this function, I suppose.
  // See error8.C for that scenario.
  static info l4 = ^^int;
  static const info l5 = ^^int;
  static constexpr info l6 = ^^int;
  static constinit info l7 = ^^int;
}
