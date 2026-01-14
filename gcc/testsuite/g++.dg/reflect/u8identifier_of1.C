// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::u8identifier_of.

#include <meta>

using namespace std::meta;

struct C { };

constexpr std::u8string_view sv = std::meta::u8identifier_of (^^C);
static_assert (sv == u8"C");
static_assert (sv.data ()[0] == u8'C');
static_assert (sv.data ()[1] == u8'\0');

struct S { };
using T = int;
using U = S;
enum E { E1, E2 };
enum { E3, E4 };
typedef enum { E5, E6 } E7;

static_assert (u8identifier_of (^^T) == std::u8string_view (u8"T"));
static_assert (u8identifier_of (^^S) == std::u8string_view (u8"S"));
static_assert (u8identifier_of (^^U) == std::u8string_view (u8"U"));
static_assert (u8identifier_of (^^std) == std::u8string_view (u8"std"));
static_assert (u8identifier_of (^^std::meta) == std::u8string_view (u8"meta"));
static_assert (u8identifier_of (^^E1) == std::u8string_view (u8"E1"));
static_assert (u8identifier_of (^^E) == std::u8string_view (u8"E"));
static_assert (u8identifier_of (parent_of (^^E5)) == std::u8string_view (u8"E7"));
static_assert (u8identifier_of (^^E7) == std::u8string_view (u8"E7"));
static_assert (u8identifier_of (data_member_spec (^^long, { .name = u8"foo", .bit_width = 6 })) == std::u8string_view (u8"foo"));
static_assert (u8identifier_of (data_member_spec (^^long, { .name = u8"extremely_long_string_used_as_identifier" })) == std::u8string_view (u8"extremely_long_string_used_as_identifier"));

namespace N {}
namespace NA = N;
static_assert (u8identifier_of (^^N) == std::u8string_view (u8"N"));
static_assert (u8identifier_of (^^NA) == std::u8string_view (u8"NA"));

namespace {
  int a;
  namespace M {}
  static_assert (u8identifier_of (^^a) == std::u8string_view (u8"a"));
  static_assert (u8identifier_of (^^M) == std::u8string_view (u8"M"));
}

typedef struct {
  int a;
  static_assert (u8identifier_of (^^a) == std::u8string_view (u8"a"));
} SV;
static_assert (u8identifier_of (^^SV) == std::u8string_view (u8"SV"));
static_assert (u8identifier_of (parent_of (^^SV::a)) == std::u8string_view (u8"SV"));
static_assert (u8identifier_of (dealias (^^SV)) == std::u8string_view (u8"SV"));

template <int N>
struct ST
{
  static_assert (u8identifier_of (^^ST) == std::u8string_view (u8"ST"));
};

struct V
{
  void foo () { int a; static_assert (u8identifier_of (parent_of (^^a)) == std::u8string_view (u8"foo")); }
  template <int N>
  void bar () { int a; static_assert (u8identifier_of (parent_of (^^a)) == std::u8string_view (u8"bar")); }
};

int
operator ""_a (const char *)
{
  int a;
  static_assert (u8identifier_of (parent_of (^^a)) == std::u8string_view (u8"_a"));
  return 0;
}

int v;
static_assert (u8identifier_of (^^V::foo) == std::u8string_view (u8"foo"));
static_assert (u8identifier_of (^^V::bar) == std::u8string_view (u8"bar"));

void foo (int);
static_assert (u8identifier_of (^^foo) == std::u8string_view (u8"foo"));

int arr[3];

void
foo (int a)
{
  auto [b, c, d] = arr;
  static_assert (u8identifier_of (^^foo) == std::u8string_view (u8"foo"));
  static_assert (u8identifier_of (^^a) == std::u8string_view (u8"a"));
  static_assert (u8identifier_of (^^b) == std::u8string_view (u8"b"));
  static_assert (u8identifier_of (^^c) == std::u8string_view (u8"c"));
  static_assert (u8identifier_of (^^d) == std::u8string_view (u8"d"));
}

template <int N>
void
bar (int a)
{
  auto [...b, c] = arr;
  static_assert (u8identifier_of (^^a) == std::u8string_view (u8"a"));
  static_assert (u8identifier_of (^^c) == std::u8string_view (u8"c"));
}

void
baz ()
{
  auto a = [] {
    int a;
    static_assert (u8identifier_of (^^a) == std::u8string_view (u8"a"));
    static_assert (u8identifier_of (parent_of (parent_of (parent_of (^^a)))) == std::u8string_view (u8"baz"));
  };
  using t = decltype (a);
  static_assert (u8identifier_of (^^t) == std::u8string_view (u8"t"));
}

void qux (int, int b, int c, int d, int);
constexpr auto p0 = parameters_of (^^qux)[0];
constexpr auto p1 = parameters_of (^^qux)[1];
constexpr auto p2 = parameters_of (^^qux)[2];
constexpr auto p3 = parameters_of (^^qux)[3];
constexpr auto p4 = parameters_of (^^qux)[4];
static_assert (u8identifier_of (p1) == std::u8string_view (u8"b"));
static_assert (u8identifier_of (p2) == std::u8string_view (u8"c"));
static_assert (u8identifier_of (p3) == std::u8string_view (u8"d"));
void qux (int a, int, int c, int e, int);
static_assert (u8identifier_of (p0) == std::u8string_view (u8"a"));
static_assert (u8identifier_of (p1) == std::u8string_view (u8"b"));
static_assert (u8identifier_of (p2) == std::u8string_view (u8"c"));

void
qux (int a, int, int, int e, int)
{
  static_assert (u8identifier_of (p0) == std::u8string_view (u8"a"));
  static_assert (u8identifier_of (p1) == std::u8string_view (u8"b"));
  static_assert (u8identifier_of (p2) == std::u8string_view (u8"c"));
  static_assert (u8identifier_of (variable_of (p0)) == std::u8string_view (u8"a"));
  static_assert (u8identifier_of (variable_of (p3)) == std::u8string_view (u8"e"));
}

void qux (int f, int, int, int, int g);
static_assert (u8identifier_of (p1) == std::u8string_view (u8"b"));
static_assert (u8identifier_of (p2) == std::u8string_view (u8"c"));
static_assert (u8identifier_of (p4) == std::u8string_view (u8"g"));

template <typename... T>
void
freddy (int a, T... b)
{
}

static_assert (u8identifier_of (parameters_of (^^freddy <int, long, char>)[0]) == std::u8string_view (u8"a"));

struct {
  int a;
} s;

static_assert (u8identifier_of (^^s) == std::u8string_view (u8"s"));

int qu\u00E6 = 1;

static_assert (u8identifier_of (^^qu\u00E6) == std::u8string_view (u8"qu\N{LATIN SMALL LETTER AE}"));

typedef enum {
  E8,
  E9
} E10;
static_assert (u8identifier_of (parent_of (^^E8)) == std::u8string_view (u8"E10"));
