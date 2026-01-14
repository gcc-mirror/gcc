// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test comparison of reflections.

#include <meta>
#include <ranges>

static union { int a; };
struct S { void foo () {} };
template <typename T>
consteval auto bar () { int v = 42; return parent_of (^^v); }
template <typename T>
consteval std::meta::info baz () { int v = 42; return parent_of (^^v); }

constexpr auto ctx = std::meta::access_context::unchecked ();
// TODO: These should work
//static_assert (^^a == members_of (parent_of (^^a), ctx)[0]);
//static_assert (^^S::~S == (members_of (^^S, ctx) | std::views::filter (std::meta::is_destructor) | std::ranges::to <std::vector> ())[0]);
static_assert (^^S::foo == members_of (^^S, ctx)[0]);
static_assert (^^bar <int> == bar <int> ());
constexpr auto b = ^^bar <long>;
static_assert (b == bar <long> ());
static_assert (^^baz <int> == baz <int> ());
constexpr auto c = ^^baz <long>;
static_assert (c == baz <long> ());
