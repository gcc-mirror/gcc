// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::define_static_array.

#include <array>
#include <meta>
#include <ranges>
#include <span>

struct V { int a, b, c; };
constexpr auto a = std::define_static_array ("abcd");
constexpr auto b = std::define_static_array (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}");
constexpr auto c = std::define_static_array (std::vector <char> {});
constexpr auto d = std::define_static_array (std::array { 1, 2, 42, 3 });
constexpr float ea[] = { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f };
constexpr auto e = std::define_static_array (ea);
constexpr int fb[] = { 1, 2, 3, 4, 5, 6 };
constexpr std::span <const int> fa (fb);
constexpr auto f = std::define_static_array (fa);
constexpr auto g = std::define_static_array (fa.subspan (1, 4));
constexpr auto h = std::define_static_array (fa.subspan (1, 4) | std::views::reverse);
constexpr auto i = std::define_static_array (std::vector <V> { V { 1, 2, 3 }, V { 2, 3, 4 }, V { 3, 4, 5 } });
constexpr auto j = std::define_static_array (std::vector <long long> {});
constexpr auto k = std::define_static_array (std::meta::nonstatic_data_members_of (^^V, std::meta::access_context::current ()));
static_assert (a.data () == std::define_static_string ("abcd") && a.size () == 5);
static_assert (b.data () == std::define_static_string (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}")
	       && b.size () == sizeof (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}") / sizeof (char32_t));
static_assert (c.data () == nullptr && c.size () == 0);
static_assert (d.data () == std::define_static_array (std::vector <int> { 1, 2, 42, 3 }).data ()
	       && d.size () == 4 && d[0] == 1 && d[1] == 2 && d[2] == 42 && d[3] == 3);
static_assert (e.data () == std::define_static_array (std::array { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f }).data ()
	       && e.size () == 6 && e[0] == 1.0f && e[1] == 2.0f && e[2] == 3.0f && e[3] == 4.0f && e[4] == 5.0f && e[5] == 6.0f);
static_assert (f.data () == std::define_static_array (std::vector <int> { 1, 2, 3, 4, 5, 6 }).data ()
	       && f.size () == 6);
static_assert (d.data () != f.data ());
static_assert (g.data () == std::define_static_array (std::array { 2, 3, 4, 5 }).data ()
	       && g.size () == 4);
static_assert (h.data () == std::define_static_array (std::vector <int> { 5, 4, 3, 2 }).data ()
	       && h.size () == 4 && h[0] == 5 && h[1] == 4 && h[2] == 3 && h[3] == 2);
static_assert (i.size () == 3);
static_assert (j.data () == nullptr && j.size () == 0);
static_assert (k.size () == 3 && k[0] == ^^V::a && k[1] == ^^V::b && k[2] == ^^V::c);

int
main ()
{
  if (std::string_view (a) != std::string_view ("abcd", 5))
    __builtin_abort ();
  if (std::u32string_view (b) != std::u32string_view (U"abcd\0ef\N{LATIN CAPITAL LETTER AE}", 9))
    __builtin_abort ();
  for (const char &x : c)
    __builtin_abort ();
  int i = 0;
  for (const int &x : d)
    if (x != "\x{1}\x{2}\x{2a}\x{3}"[i++])
      __builtin_abort ();
  if (i != 4)
    __builtin_abort ();
  i = 0;
  for (const float &x : e)
    if (x != ++i)
      __builtin_abort ();
  if (i != 6)
    __builtin_abort ();
  i = 0;
  for (const int &x : f)
    if (x != ++i)
      __builtin_abort ();
  if (i != 6)
    __builtin_abort ();
  i = 1;
  for (const int &x : g)
    if (x != ++i)
      __builtin_abort ();
  if (i != 5)
    __builtin_abort ();
  i = 6;
  for (const int &x : h)
    if (x != --i)
      __builtin_abort ();
  if (i != 2)
    __builtin_abort ();
  i = 0;
  for (const V &x : ::i)
    if (x.a != i + 1 || x.b != i + 2 || x.c != i + 3)
      __builtin_abort ();
    else
      ++i;
  if (i != 3)
    __builtin_abort ();
}
