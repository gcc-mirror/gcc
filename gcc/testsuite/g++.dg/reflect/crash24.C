// PR c++/128855
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct NS {
  constexpr NS () {}
private:
  int x = 0;
};
constexpr NS ns;

constexpr auto spec
  = std::meta::data_member_spec (^^int, { .name = "m",	// { dg-error "must be a cv-unqualified structural type" }
					   .annotations = { ^^ns } });

constexpr auto s = std::meta::display_string_of (spec);

struct A;
consteval { std::meta::define_aggregate (^^A, { spec }); }

constexpr NS x = std::meta::extract<NS> (^^ns); // { dg-error "must be a cv-unqualified structural type" }
