// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test which constructs or metafunctions implicitly instantiate.

#include <meta>

template <int N>
struct S {
  static_assert (N < 42);	// { dg-line static_assert }
};
// { dg-error "static assertion failed" "" { target *-*-* } static_assert }
// { dg-bogus "the comparison reduces to '\\\(42 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(43 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(44 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-bogus "the comparison reduces to '\\\(45 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-bogus "the comparison reduces to '\\\(46 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(47 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(48 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(49 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(50 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(51 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(52 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(53 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(54 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(55 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(56 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(57 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-bogus "the comparison reduces to '\\\(58 < 42\\\)'" "" { target *-*-* } static_assert }
// { dg-message "the comparison reduces to '\\\(59 < 42\\\)'" "" { target *-*-* } static_assert }

S<0> s1;
constexpr auto a = ^^S<42>;
bool b = is_complete_type (^^S<43>);
bool c = is_enumerable_type (^^S<44>);
constexpr auto d = data_member_spec (^^S<45>, { .name = "_" });
consteval {
  define_aggregate (^^S<46>, { data_member_spec (^^int, { .name = "_" }) });
}
auto e = size_of (^^S<47>);
auto f = bit_size_of (^^S<48>);
int g;
constexpr auto h = annotations_of_with_type (^^g, ^^S<49>);
constexpr auto ctx = std::meta::access_context::current ();
auto i = members_of (^^S<50>, ctx).size ();
auto j = bases_of (^^S<51>, ctx).size ();
auto k = static_data_members_of (^^S<52>, ctx).size ();
auto l = nonstatic_data_members_of (^^S<53>, ctx).size ();
auto m = subobjects_of (^^S<54>, ctx).size ();
bool n = has_inaccessible_nonstatic_data_members (^^S<55>, ctx);
bool o = has_inaccessible_bases (^^S<56>, ctx);
bool p = has_inaccessible_subobjects (^^S<57>, ctx);
consteval {
  define_aggregate (^^S<58>, { data_member_spec (^^S<59>, { .name = "_" }) });
}
