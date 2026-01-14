// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we properly detect using consteval-only variables without
// constexpr/constinit.

using info = decltype(^^int);

info r1 = ^^int;  // { dg-error "consteval-only variable .r1. not declared .constexpr. used outside a constant-evaluated context" }
const info r2 = ^^int;  // { dg-error "consteval-only variable .r2. not declared .constexpr. used outside a constant-evaluated context" }

constexpr info r3 = ^^int;
constinit info r4 = ^^int;
const info *const p1 = &r3;  // { dg-error "consteval-only variable .p1. not declared .constexpr. used outside a constant-evaluated context" }
info *p2;  // { dg-error "consteval-only variable .p2. not declared .constexpr. used outside a constant-evaluated context" }
const info &q = r3;  // { dg-error "consteval-only variable .q. not declared .constexpr. used outside a constant-evaluated context" }

void
g ()
{
  info l1 = ^^int;  // { dg-error "consteval-only variable .l1. not declared .constexpr. used outside a constant-evaluated context" }
  const info l2 = ^^int;  // { dg-error "consteval-only variable .l2. not declared .constexpr. used outside a constant-evaluated context" }
  constexpr info l3 = ^^int;
  static info l4 = ^^int;  // { dg-error "consteval-only variable .l4. not declared .constexpr. used outside a constant-evaluated context" }
  static const info l5 = ^^int;  // { dg-error "consteval-only variable .l5. not declared .constexpr. used outside a constant-evaluated context" }
  static constexpr info l6 = ^^int;
  static constinit info l7 = ^^int;
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
