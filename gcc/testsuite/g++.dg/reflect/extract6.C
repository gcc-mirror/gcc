// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

[[=1, =1, =2, =1.0f]] void fn ();
struct [[=3, =3, =4, =2.0f]] S;

template<info R>
[[=[:constant_of (annotations_of (R)[0]):]]] void TFn();
template<info R>
struct [[=[:constant_of (annotations_of (R)[0]):]]] TCls {};

static_assert (extract<int>(annotations_of (^^TFn<^^::fn>)[0]) == 1);
static_assert (extract<int>(annotations_of (^^TCls<^^::S>)[0]) == 3); // { dg-error "non-constant" }
static_assert (extract<int>(annotations_of (^^fn)[0]) == 1);
// { dg-error "call to non-.constexpr." "" { target *-*-* } 0 }
