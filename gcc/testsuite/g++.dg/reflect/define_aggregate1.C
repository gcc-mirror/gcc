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
union U1;
template <int N>
struct S6;
template <int N>
struct S7 { long e; short f; };
struct S8;
struct S9;
using A9 = S9;

consteval {
  if (define_aggregate (^^S1, {}) != ^^S1)
    throw 1;
  if (define_aggregate (^^S2, { data_member_spec (^^S1, { .name = "bar" }),
				data_member_spec (^^long, { .name = u8"baz",
							    .alignment = 2 * alignof (long),
							    .no_unique_address = true }),
				data_member_spec (^^unsigned int, { .bit_width = 7 }),
				data_member_spec (^^E, { .name = "extremely_long_identifier1",
							 .bit_width = 6 }),
				data_member_spec (^^int, { .bit_width = 0 }),
				data_member_spec (^^const E *, { .name = u8"extremely_long_identifier2",
								 .alignment = 2 * alignof (E *) }) }) != ^^S2)
    throw 2;
  if (define_aggregate (^^S3, { data_member_spec (^^S0, { .name = "a",
							  .no_unique_address = true }),
				data_member_spec (^^S1, { .name = "b",
							  .no_unique_address = true }) }) != ^^S3)
    throw 3;
  if (define_aggregate (^^S4, { data_member_spec (^^S0, { .name = "c" }),
				data_member_spec (^^S1, { .name = "d" }) }) != ^^S4)
    throw 4;
  if (define_aggregate (^^S5, { data_member_spec (^^const E &, { .name = u8"qu\N{LATIN SMALL LETTER AE}" }),
				data_member_spec (^^const E &, { .name = u8"foo" }) }) != ^^S5)
    throw 5;
  if (define_aggregate (^^U1, { data_member_spec (^^int, { .name = u8"_" }),
				data_member_spec (^^long long, { .name = "abc" }) }) != ^^U1)
    throw 6;
  if (define_aggregate (^^S6 <42>, { data_member_spec (^^int, { .name = "a" }),
				     data_member_spec (^^long, { .name = "b" }) }) != ^^S6 <42>)
    throw 7;
  if (define_aggregate (substitute (^^S6, { reflect_constant (43) }),
			{ data_member_spec (^^long, { .name = "c" }),
			  data_member_spec (^^unsigned int, { .name = "d", .bit_width = 3 }) }) != ^^S6 <43>)
    throw 8;
  if (define_aggregate (^^S7 <42>, { data_member_spec (^^short, { .name = "g" }),
				     data_member_spec (^^float, { .name = "h" }) }) != ^^S7 <42>)
    throw 9;
  if (define_aggregate (^^S8, { data_member_spec (^^U1, { .name = "u" }) }) != ^^S8)
    throw 10;
  if (define_aggregate (dealias (^^A9), { data_member_spec (^^U1, { .name = "u" }) }) != ^^S9)
    throw 10;
}

constexpr E e0 = E0, e1 = E1;
S2 s2 = { .bar = {}, .baz = 42LL, .extremely_long_identifier1 = E1, .extremely_long_identifier2 = &e0 };
S3 s3 = { .a = {}, .b = {} };
S4 s4 = { .c = {}, .d = {} };
constexpr S5 s5 = { e0, e1 };
U1 u1 = { ._ = 5 }, u1a = { .abc = 42LL };
S6 <42> s642 = { .a = 1, .b = 2 };
S6 <43> s643 = { .c = 6, .d = 7 };
S7 <42> s742 = { .g = 5, .h = 6.0f };
S7 <43> s743 = { .e = 8, .f = 9 };
S8 s8 = { .u = { .abc = 2LL } };
S9 s9 = { .u = { ._ = 3 } };
consteval {
  S6 <43> x;
  x.c = -1;
  x.d = 7;
  ++x.d;
  if (x.d != 0)
    throw 11;
  --x.d;
  if (x.d != 7)
    throw 12;
}
static_assert (type_of (^^S2::bar) == ^^S1);
static_assert (type_of (^^S2::baz) == ^^long);
static_assert (type_of (^^S2::extremely_long_identifier1) == ^^E);
static_assert (type_of (^^S2::extremely_long_identifier2) == ^^const E *);
static_assert (offsetof (S2, bar) == 0);
static_assert (offsetof (S2, baz) >= 2 * alignof (long) && offsetof (S2, baz) % (2 * alignof (long)) == 0);
static_assert (offsetof (S2, extremely_long_identifier2) > offsetof (S2, baz));
static_assert (offsetof (S2, extremely_long_identifier2) % (2 * alignof (E)) == 0);
static_assert (type_of (^^S3::a) == ^^S0);
static_assert (type_of (^^S3::b) == ^^S1);
static_assert (sizeof (S3) == sizeof (S0) && sizeof (S4) == 2 * sizeof (S0));
static_assert (type_of (^^S4::c) == ^^S0);
static_assert (type_of (^^S4::d) == ^^S1);
static_assert (s5.qu\u00E6 == E0 && s5.foo == E1);
static_assert (type_of (^^S5::qu\u00E6) == ^^const E &);
static_assert (type_of (^^S5::foo) == ^^const E &);
