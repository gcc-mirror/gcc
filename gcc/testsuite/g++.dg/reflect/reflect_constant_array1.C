// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_array.

#include <array>
#include <meta>
#include <ranges>
#include <span>

using namespace std::meta;

struct V { int a, b, c; };
constexpr auto a = reflect_constant_array ("abcd");
constexpr auto b = reflect_constant_array (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}");
constexpr auto c = reflect_constant_array (std::vector <char> {});
constexpr auto d = reflect_constant_array (std::array { 1, 2, 42, 3 });
constexpr float ea[] = { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f };
constexpr auto e = reflect_constant_array (ea);
constexpr int fb[] = { 1, 2, 3, 4, 5, 6 };
constexpr std::span <const int> fa (fb);
constexpr auto f = reflect_constant_array (fa);
constexpr auto g = reflect_constant_array (fa.subspan (1, 4));
constexpr auto h = reflect_constant_array (fa.subspan (1, 4) | std::views::reverse);
constexpr auto i = reflect_constant_array (std::vector <V> { V { 1, 2, 3 }, V { 2, 3, 4 }, V { 3, 4, 5 } });
static_assert (a == reflect_constant_string ("abcd"));
static_assert (b == reflect_constant_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"));
static_assert (c == reflect_constant_array (std::vector <char> {}));
static_assert (d == reflect_constant_array (std::vector <int> { 1, 2, 42, 3 }));
static_assert (e == reflect_constant_array (std::array { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f }));
static_assert (f == reflect_constant_array (std::vector <int> { 1, 2, 3, 4, 5, 6 }));
static_assert (d != f);
static_assert (g == reflect_constant_array (std::array { 2, 3, 4, 5 }));
static_assert (h == reflect_constant_array (std::vector <int> { 5, 4, 3, 2 }));
//static_assert (i == reflect_constant_array (std::vector <V> { V { 1, 2, 3 }, V { 2, 3, 4 }, V { 3, 4, 5 } }));
static_assert (is_variable (a));
static_assert (is_variable (b));
static_assert (is_variable (c));
static_assert (is_variable (d));
static_assert (is_variable (e));
static_assert (is_variable (f));
static_assert (is_variable (g));
static_assert (is_variable (h));
static_assert (is_variable (i));
static_assert (type_of (a) == ^^const char [5]);
static_assert (type_of (b) == ^^const char32_t [9]);
static_assert (type_of (c) == ^^const std::array <char, 0>);
static_assert (type_of (d) == ^^const int [4]);
static_assert (type_of (e) == ^^const float [6]);
static_assert (type_of (f) == ^^const int [6]);
static_assert (type_of (g) == ^^const int [4]);
static_assert (type_of (h) == ^^const int [4]);
static_assert (type_of (i) == ^^const V [3]);
auto as = &[: a :];
auto bs = &[: b :];
auto cs = &[: c :];
auto ds = &[: d :];
auto es = &[: e :];
auto fs = &[: f :];
auto gs = &[: g :];
auto hs = &[: h :];
auto is = &[: i :];

int
main ()
{
  if (std::string_view (*as) != std::string_view ("abcd"))
    __builtin_abort ();
  if (std::u32string_view (*bs) != std::u32string_view (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"))
    __builtin_abort ();
  for (const char &x : *cs)
    __builtin_abort ();
  int i = 0;
  for (const int &x : *ds)
    if (x != "\x{1}\x{2}\x{2a}\x{3}"[i++])
      __builtin_abort ();
  if (i != 4)
    __builtin_abort ();
  i = 0;
  for (const float &x : *es)
    if (x != ++i)
      __builtin_abort ();
  if (i != 6)
    __builtin_abort ();
  i = 0;
  for (const int &x : *fs)
    if (x != ++i)
      __builtin_abort ();
  if (i != 6)
    __builtin_abort ();
  i = 1;
  for (const int &x : *gs)
    if (x != ++i)
      __builtin_abort ();
  if (i != 5)
    __builtin_abort ();
  i = 6;
  for (const int &x : *hs)
    if (x != --i)
      __builtin_abort ();
  if (i != 2)
    __builtin_abort ();
  i = 0;
  for (const V &x : *is)
    if (x.a != i + 1 || x.b != i + 2 || x.c != i + 3)
      __builtin_abort ();
    else
      ++i;
  if (i != 3)
    __builtin_abort ();
}
