// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>
#include <ranges>

struct foo;

consteval {
  using namespace std::meta;
  using namespace std::views;
  using std::make_pair;
  using std::vector;
  using std::pair;
  define_aggregate (^^foo, join (vector <vector <pair <bool, info>>> {
    { make_pair (true, data_member_spec (^^int, { .name = "i" })) },
    { make_pair (false, data_member_spec (^^std::string, { .name = "s" })),
      make_pair (true, data_member_spec (^^bool, { .name = "b" })) }})
	| filter ([] (auto x) constexpr { return x.first; })
	| transform ([] (auto x) constexpr { return x.second; }));
}

static_assert (type_of (^^foo::i) == ^^int);
static_assert (type_of (^^foo::b) == ^^bool);
static_assert (nonstatic_data_members_of (^^foo, std::meta::access_context::current ()).size () == 2);
