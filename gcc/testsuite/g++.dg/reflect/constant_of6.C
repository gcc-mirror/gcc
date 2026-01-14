// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

[[=1, =1, =2, =1.0f]] void fn();
struct [[=3, =3, =4, =2.0f]] S;

template<info R>
[[=[:constant_of (annotations_of (R)[0]):]]] void bar();

template<info R>
struct [[=[:constant_of (annotations_of (R)[0]):]]] Y {};

constexpr auto y = constant_of (annotations_of (^^bar<^^::fn>)[0]);
constexpr auto z = constant_of (annotations_of (^^Y<^^::S>)[0]);
// { dg-error "call to non-.constexpr." "" { target *-*-* } 0 }
