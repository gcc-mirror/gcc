// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::define_static_string.

#include <meta>
#include <ranges>
#include <span>

constexpr const char *a = std::define_static_string ("abcd");
constexpr const char8_t *b = std::define_static_string (u8"abcd\N{LATIN SMALL LETTER AE}");
constexpr const wchar_t *c = std::define_static_string (L"abcd");
constexpr const char16_t *d = std::define_static_string (u"abcd\0ef");
constexpr const char32_t *e = std::define_static_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}");
constexpr auto f = std::define_static_string (std::string_view ("abcd", 5));
constexpr auto g = std::define_static_string (std::string_view ("abcdefg", 4));
constexpr auto h = std::define_static_string (std::u8string_view (u8"ab\0\N{LATIN SMALL LETTER AE}", 5));
constexpr auto i = std::define_static_string (std::string_view ("abcdefgh", 9).substr (0, 5));
constexpr auto j = std::define_static_string (std::string_view ("abcdefgh", 9).substr (4, 3));
constexpr auto k = std::define_static_string (std::u32string_view (U"abcdefgh", 9).substr (3, 4));
constexpr char8_t la[] = { u8'h', u8'e', u8'l', u8'l', u8'o', u8'\0' };
constexpr auto l = std::define_static_string (la);
constexpr std::span <const char8_t> ma (la);
constexpr auto m = std::define_static_string (ma);
constexpr auto n = std::define_static_string (ma.subspan (1, 4));
constexpr auto o = std::define_static_string (ma.subspan (1, 4) | std::views::reverse);
constexpr auto p = std::define_static_string (std::vector <wchar_t> { L'W', L'o', L'r', L'l', L'd' });
constexpr auto q = std::define_static_string (std::vector <char16_t> { u'e', u'x', u't', u'r', u'e', u'm', u'e', u'l', u'y',
								       u' ', u'l', u'o', u'n', u'g',
								       u' ', u's', u't', u'r', u'i', u'n', u'g',
								       u' ', u'w', u'i', u't', u'h',
								       u' ', u'n', u'o', u'n', u'-', u'A', u'S', u'C', u'I', u'I',
								       u' ', u'c', u'h', u'a', u'r', u'a', u'c', u't', u'e', u'r', u's',
								       u' ', u'\N{LATIN SMALL LETTER A WITH ACUTE}' });
const char *r = std::define_static_string ("some string");
const char8_t *s = std::define_static_string (u8"\N{GRINNING FACE}\N{GRINNING FACE WITH SMILING EYES}");
static_assert (a == std::define_static_string ("abcd"));
static_assert (b == std::define_static_string (u8"abcd\N{LATIN SMALL LETTER AE}"));
static_assert (c == std::define_static_string (L"abcd"));
static_assert (d == std::define_static_string (u"abcd\0ef"));
static_assert (e == std::define_static_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"));
static_assert (f == std::define_static_string ("abcd\0"));
static_assert (g == a);
static_assert (h == std::define_static_string (u8"ab\0\N{LATIN SMALL LETTER AE}"));
static_assert (i == std::define_static_string ("abcde"));
static_assert (a != i);
static_assert (j == std::define_static_string ("efg"));
static_assert (a != j);
static_assert (k == std::define_static_string (U"defg"));
static_assert (l == std::define_static_string (u8"hello\0"));
static_assert (m == l);
static_assert (n == std::define_static_string (u8"ello"));
static_assert (o == std::define_static_string (u8"olle"));
static_assert (p == std::define_static_string (L"World"));
static_assert (q == std::define_static_string (u"extremely long string with non-ASCII characters \N{LATIN SMALL LETTER A WITH ACUTE}"));
static_assert (std::define_static_string ("bar") != std::define_static_string ("baz"));

template <typename T, const T *P>
struct C { const T *p = P; };

static_assert (std::is_same_v <C <char, std::define_static_string ("foobar")>,
			       C <char, std::define_static_string (std::vector <char> { 'f', 'o', 'o', 'b', 'a', 'r' })>>);
static_assert (!std::is_same_v <C <char, std::define_static_string ("foobar")>,
				C <char, std::define_static_string (std::vector <char> { 'f', 'o', 'O', 'b', 'a', 'r' })>>);
static_assert (std::is_same_v <C <wchar_t, std::define_static_string (L"hello")>,
			       C <wchar_t, std::define_static_string (L"hello")>>);
static_assert (std::is_same_v <C <char8_t, std::define_static_string (u8"hello\0")>,
			       C <char8_t, std::define_static_string (ma)>>);
static_assert (std::is_same_v <C <char8_t, std::define_static_string (u8"\N{LATIN SMALL LETTER AE}")>,
			       C <char8_t, std::define_static_string (u8"\N{LATIN SMALL LETTER AE}")>>);
static_assert (std::is_same_v <C <char16_t, std::define_static_string (u"\N{LATIN CAPITAL LETTER AE}")>,
			       C <char16_t, std::define_static_string (std::vector <char16_t> { u'\N{LATIN CAPITAL LETTER AE}' })>>);
static_assert (std::is_same_v <C <char32_t, std::define_static_string (U"\N{GRINNING FACE}\N{GRINNING FACE WITH SMILING EYES}")>,
			       C <char32_t, std::define_static_string (std::vector <char32_t> { U'\N{GRINNING FACE}', U'\N{GRINNING FACE WITH SMILING EYES}' })>>);

template <auto V>
consteval auto
foo ()
{
  return V[0];
}

static_assert (foo <std::define_static_string ("foo")> () == 'f');
static_assert (foo <std::define_static_string (L"bar")> () == L'b');
static_assert (foo <std::define_static_string (u8"qux")> () == u8'q');
static_assert (foo <std::define_static_string (u"\N{LATIN SMALL LETTER AE}\N{LATIN CAPITAL LETTER AE}")> () == u'\N{LATIN SMALL LETTER AE}');
static_assert (foo <std::define_static_string (U"\N{GRINNING FACE WITH SMILING EYES}\N{GRINNING FACE}")> () == U'\N{GRINNING FACE WITH SMILING EYES}');

int
main ()
{
  if (std::string_view (a) != std::string_view ("abcd"))
    __builtin_abort ();
  if (std::u8string_view (b) != std::u8string_view (u8"abcd\N{LATIN SMALL LETTER AE}"))
    __builtin_abort ();
  if (std::wstring_view (c) != std::wstring_view (L"abcd"))
    __builtin_abort ();
  if (std::u16string_view (d) != std::u16string_view (u"abcd\0ef"))
    __builtin_abort ();
  if (std::u32string_view (e) != std::u32string_view (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}"))
    __builtin_abort ();
  if (std::string_view (f) != std::string_view ("abcd\0"))
    __builtin_abort ();
  if (g != a)
    __builtin_abort ();
  if (std::u8string_view (h) != std::u8string_view (u8"ab\0\N{LATIN SMALL LETTER AE}"))
    __builtin_abort ();
  if (std::string_view (i) != std::string_view ("abcde"))
    __builtin_abort ();
  if (a == i)
    __builtin_abort ();
  if (std::string_view (j) != std::string_view ("efg"))
    __builtin_abort ();
  if (a == j)
    __builtin_abort ();
  if (std::u32string_view (k) != std::u32string_view (U"defg"))
    __builtin_abort ();
  if (std::u8string_view (l) != std::u8string_view (u8"hello\0"))
    __builtin_abort ();
  if (m != l)
    __builtin_abort ();
  if (std::u8string_view (n) != std::u8string_view (u8"ello"))
    __builtin_abort ();
  if (std::u8string_view (o) != std::u8string_view (u8"olle"))
    __builtin_abort ();
  if (std::wstring_view (p) != std::wstring_view (L"World"))
    __builtin_abort ();
  if (std::u16string_view (q) != std::u16string_view (u"extremely long string with non-ASCII characters \N{LATIN SMALL LETTER A WITH ACUTE}"))
    __builtin_abort ();
  if (r != std::define_static_string ("some string"))
    __builtin_abort ();
  if (s != std::define_static_string (u8"\N{GRINNING FACE}\N{GRINNING FACE WITH SMILING EYES}"))
    __builtin_abort ();
}
