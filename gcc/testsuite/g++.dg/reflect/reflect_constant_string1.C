// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_string.

#include <meta>
#include <ranges>
#include <span>

using namespace std::meta;

constexpr auto a = reflect_constant_string ("abcd");
constexpr auto b = reflect_constant_string (u8"abcd\N{LATIN SMALL LETTER AE}");
constexpr auto c = reflect_constant_string (L"abcd");
constexpr auto d = reflect_constant_string (u"abcd\0ef");
constexpr auto e = reflect_constant_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}");
constexpr auto f = reflect_constant_string (std::string_view ("abcd", 5));
constexpr auto g = reflect_constant_string (std::string_view ("abcdefg", 4));
constexpr auto h = reflect_constant_string (std::u8string_view (u8"ab\0\N{LATIN SMALL LETTER AE}", 5));
constexpr auto i = reflect_constant_string (std::string_view ("abcdefgh", 9).substr (0, 5));
constexpr auto j = reflect_constant_string (std::string_view ("abcdefgh", 9).substr (4, 3));
constexpr auto k = reflect_constant_string (std::u32string_view (U"abcdefgh", 9).substr (3, 4));
constexpr char8_t la[] = { u8'h', u8'e', u8'l', u8'l', u8'o', u8'\0' };
constexpr auto l = reflect_constant_string (la);
constexpr std::span <const char8_t> ma (la);
constexpr auto m = reflect_constant_string (ma);
constexpr auto n = reflect_constant_string (ma.subspan (1, 4));
constexpr auto o = reflect_constant_string (ma.subspan (1, 4) | std::views::reverse);
constexpr auto p = reflect_constant_string (std::vector <wchar_t> { L'W', L'o', L'r', L'l', L'd' });
constexpr auto q = reflect_constant_string (std::vector <char16_t> { u'e', u'x', u't', u'r', u'e', u'm', u'e', u'l', u'y',
								     u' ', u'l', u'o', u'n', u'g',
								     u' ', u's', u't', u'r', u'i', u'n', u'g',
								     u' ', u'w', u'i', u't', u'h',
								     u' ', u'n', u'o', u'n', u'-', u'A', u'S', u'C', u'I', u'I',
								     u' ', u'c', u'h', u'a', u'r', u'a', u'c', u't', u'e', u'r', u's',
								     u' ', u'\N{LATIN SMALL LETTER A WITH ACUTE}' });
static_assert (a == reflect_constant_string ("abcd"));
static_assert (b == reflect_constant_string (u8"abcd\N{LATIN SMALL LETTER AE}"));
static_assert (c == reflect_constant_string (L"abcd"));
static_assert (d == reflect_constant_string (u"abcd\0ef"));
static_assert (e == reflect_constant_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"));
static_assert (f == reflect_constant_string ("abcd\0"));
static_assert (a != f);
static_assert (g == a);
static_assert (h == reflect_constant_string (u8"ab\0\N{LATIN SMALL LETTER AE}"));
static_assert (i == reflect_constant_string ("abcde"));
static_assert (a != i);
static_assert (j == reflect_constant_string ("efg"));
static_assert (a != j);
static_assert (k == reflect_constant_string (U"defg"));
static_assert (l == reflect_constant_string (u8"hello\0"));
static_assert (m == l);
static_assert (n == reflect_constant_string (u8"ello"));
static_assert (o == reflect_constant_string (u8"olle"));
static_assert (p == reflect_constant_string (L"World"));
static_assert (q == reflect_constant_string (u"extremely long string with non-ASCII characters \N{LATIN SMALL LETTER A WITH ACUTE}"));
static_assert (reflect_constant_string ("bar") != reflect_constant_string ("baz"));
static_assert (is_variable (a));
static_assert (is_variable (b));
static_assert (is_variable (c));
static_assert (is_variable (d));
static_assert (is_variable (e));
static_assert (is_variable (f));
static_assert (is_variable (g));
static_assert (is_variable (h));
static_assert (type_of (a) == ^^const char [5]);
static_assert (type_of (b) == ^^const char8_t [sizeof u8"abcd\N{LATIN SMALL LETTER AE}"]);
static_assert (type_of (c) == ^^const wchar_t [5]);
static_assert (type_of (d) == ^^const char16_t [8]);
static_assert (type_of (e) == ^^const char32_t [9]);
static_assert (type_of (f) == ^^const char [6]);
static_assert (type_of (g) == ^^const char [5]);
static_assert (type_of (h) == ^^const char8_t [6]);
static_assert (type_of (q) == ^^const char16_t [50]);
auto as = &[: a :];
auto bs = &[: b :];
auto cs = &[: c :];
auto ds = &[: d :];
auto es = &[: e :];
auto fs = &[: f :];
auto gs = &[: g :];
auto hs = &[: h :];
auto is = &[: i :];
auto js = &[: j :];
auto ks = &[: k :];
auto ls = &[: l :];
auto ms = &[: m :];
auto ns = &[: n :];
auto os = &[: o :];
auto ps = &[: p :];
auto qs = &[: q :];

int
main ()
{
  if (std::string_view (*as) != std::string_view ("abcd"))
    __builtin_abort ();
  if (std::u8string_view (*bs) != std::u8string_view (u8"abcd\N{LATIN SMALL LETTER AE}"))
    __builtin_abort ();
  if (std::wstring_view (*cs) != std::wstring_view (L"abcd"))
    __builtin_abort ();
  if (std::u16string_view (*ds) != std::u16string_view (u"abcd\0ef"))
    __builtin_abort ();
  if (std::u32string_view (*es) != std::u32string_view (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"))
    __builtin_abort ();
  if (std::string_view (*fs) != std::string_view ("abcd\0"))
    __builtin_abort ();
  if (gs != as)
    __builtin_abort ();
  if (std::u8string_view (*hs) != std::u8string_view (u8"ab\0\N{LATIN SMALL LETTER AE}"))
    __builtin_abort ();
  if (std::string_view (*is) != std::string_view ("abcde"))
    __builtin_abort ();
  if ((const char *) as == (const char *) is)
    __builtin_abort ();
  if (std::string_view (*js) != std::string_view ("efg"))
    __builtin_abort ();
  if ((const char *) as == (const char *) js)
    __builtin_abort ();
  if (std::u32string_view (*ks) != std::u32string_view (U"defg"))
    __builtin_abort ();
  if (std::u8string_view (*ls) != std::u8string_view (u8"hello\0"))
    __builtin_abort ();
  if (ms != ls)
    __builtin_abort ();
  if (std::u8string_view (*ns) != std::u8string_view (u8"ello"))
    __builtin_abort ();
  if (std::u8string_view (*os) != std::u8string_view (u8"olle"))
    __builtin_abort ();
  if (std::wstring_view (*ps) != std::wstring_view (L"World"))
    __builtin_abort ();
  if (std::u16string_view (*qs) != std::u16string_view (u"extremely long string with non-ASCII characters \N{LATIN SMALL LETTER A WITH ACUTE}"))
    __builtin_abort ();
}
