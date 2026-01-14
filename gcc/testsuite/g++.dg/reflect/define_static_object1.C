// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::define_static_object.

#include <meta>

struct V { int a, b, c; };
constexpr auto a = std::define_static_object (42);
constexpr auto b = std::define_static_object (U'\N{LATIN CAPITAL LETTER AE}');
constexpr auto c = std::define_static_object (V { 1, 2, 42 });
static_assert (a == std::define_static_object (42) && *a == 42);
static_assert (is_same_type (type_of (^^a), ^^const int *const));
static_assert (b == std::define_static_object (U'\N{LATIN CAPITAL LETTER AE}') && *b == U'\N{LATIN CAPITAL LETTER AE}');
static_assert (is_same_type (type_of (^^b), ^^const char32_t *const));
static_assert (c == std::define_static_object (V { 1, 2, 42 }) && c->a == 1 && c->b == 2 && c->c == 42);
static_assert (is_same_type (type_of (^^c), ^^const V *const));
static_assert (a == std::define_static_array (std::vector <int> { 42 }).data ());
static_assert (b == std::define_static_array (std::vector <char32_t> { U'\N{LATIN CAPITAL LETTER AE}' }).data ());
