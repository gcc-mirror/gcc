// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::define_aggregate.

#include <cstddef>
#include <meta>

using namespace std::meta;

enum E { E0, E1 };
struct S0 {};
struct S1;
struct S2;
struct S3;
struct S4;
struct S5;
template <int N>
struct S7 { long e; short f; };
S7 <15> s715;
struct S8;
using A8 = S8;
struct S9;
struct S10;
struct S11;
struct S12;
consteval { define_aggregate (^^::, {}); }		// { dg-error "first 'define_aggregate' argument is not a class type reflection" }
consteval { define_aggregate (^^int, {}); }		// { dg-error "first 'define_aggregate' argument is not a class type reflection" }
consteval { define_aggregate (^^E, {}); }		// { dg-error "first 'define_aggregate' argument is not a class type reflection" }
consteval { define_aggregate (^^S0, {}); }		// { dg-error "first 'define_aggregate' argument is a complete class type reflection" }
consteval { define_aggregate (^^S7 <15>, {}); }		// { dg-error "first 'define_aggregate' argument is a complete class type reflection" }
consteval { define_aggregate (^^A8, {}); }		// { dg-error "first 'define_aggregate' argument is a reflection of a type alias" }
consteval { define_aggregate (^^const S9, {}); }	// { dg-error "first 'define_aggregate' argument is a cv-qualified class type reflection" }
consteval { define_aggregate (^^volatile S10, {}); }	// { dg-error "first 'define_aggregate' argument is a cv-qualified class type reflection" }
consteval { define_aggregate (^^S11 const volatile, {}); } // { dg-error "first 'define_aggregate' argument is a cv-qualified class type reflection" }
consteval { define_aggregate (^^S1, { ^^int }); }	// { dg-error "'define_aggregate' argument not a data member description" }
consteval { define_aggregate (^^S2, { data_member_spec (^^S5, { .name = "a" }) }); }	// { dg-error "'define_aggregate' argument data member description without complete type" }
consteval { define_aggregate (^^S3, { data_member_spec (^^int, { .name = "a" }),	// { dg-error "name 'a' used in multiple data member descriptions" }
				      data_member_spec (^^long, { .name = "a" }) }); }
consteval { define_aggregate (^^S4, { data_member_spec (^^int, { .name = u8"_" }),
				      data_member_spec (^^long, { .name = u8"_" }) }); }
consteval { define_aggregate (^^S12, { data_member_spec (^^int, { .name = "foobar" }),	// { dg-error "name 'foobar' used in multiple data member descriptions" }
				       data_member_spec (^^long, { .name = u8"foobar" }) }); }
constexpr S4 s4 = { 1, 2 };
consteval { auto a = s4._; }				// { dg-error "request for member '_' is ambiguous" }
