// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

// [expr.const.const]

consteval int plus1(int x) { return x + 1; }
template <auto V> struct C { };

auto a = plus1; // { dg-error "taking address of an immediate function" }
constexpr auto b = plus1; // // { dg-bogus "returns address" "" { xfail *-*-* } } OK
auto c = C<plus1>(); // OK

auto d = ^^int; // { dg-error "initialized with a consteval-only value" }
auto e = C<^^char>(); // OK

// [expr.const.imm]

consteval info refl() { return ^^int; }
template <auto F>
constexpr void ex() {
  auto x = F();
}
auto p = &ex<refl>; // { dg-error "taking address of an immediate function" }

consteval int id(int x) { return x; }
template <auto F>
constexpr auto apply_to(int i) { return F(i); }
auto q = &apply_to<id>; // { dg-error "taking address of an immediate function" }
