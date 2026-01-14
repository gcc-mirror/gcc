// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::identifier_of.

#include <meta>

using namespace std::meta;

struct C { };

constexpr std::string_view sv = identifier_of (^^C);
static_assert (sv == "C");
static_assert (sv.data ()[0] == 'C');
static_assert (sv.data ()[1] == '\0');

struct S { };
using T = int;
using U = S;
enum E { E1, E2 };
enum { E3, E4 };
typedef enum { E5, E6 } E7;

static_assert (identifier_of (^^T) == std::string_view ("T"));
static_assert (identifier_of (^^S) == std::string_view ("S"));
static_assert (identifier_of (^^U) == std::string_view ("U"));
static_assert (identifier_of (^^std) == std::string_view ("std"));
static_assert (identifier_of (^^std::meta) == std::string_view ("meta"));
static_assert (identifier_of (^^E1) == std::string_view ("E1"));
static_assert (identifier_of (^^E) == std::string_view ("E"));
static_assert (identifier_of (parent_of (^^E5)) == std::string_view ("E7"));
static_assert (identifier_of (^^E7) == std::string_view ("E7"));
static_assert (identifier_of (dealias (^^E7)) == std::string_view ("E7"));
static_assert (identifier_of (data_member_spec (^^long, { .name = "foo", .bit_width = 6 })) == std::string_view ("foo"));
static_assert (identifier_of (data_member_spec (^^long, { .name = "extremely_long_string_used_as_identifier" })) == std::string_view ("extremely_long_string_used_as_identifier"));

namespace N {}
namespace NA = N;
static_assert (identifier_of (^^N) == std::string_view ("N"));
static_assert (identifier_of (^^NA) == std::string_view ("NA"));

namespace {
  int a;
  namespace M {}
  static_assert (identifier_of (^^a) == std::string_view ("a"));
  static_assert (identifier_of (^^M) == std::string_view ("M"));
}

typedef struct {
  int a;
  static_assert (identifier_of (^^a) == std::string_view ("a"));
} SV;
static_assert (identifier_of (^^SV) == std::string_view ("SV"));
static_assert (identifier_of (parent_of (^^SV::a)) == std::string_view ("SV"));
static_assert (identifier_of (dealias (^^SV)) == std::string_view ("SV"));

template <int N>
struct ST
{
  static_assert (identifier_of (^^ST) == std::string_view ("ST"));
};
static_assert (identifier_of (^^ST) == std::string_view ("ST"));

struct V
{
  void foo () { int a; static_assert (identifier_of (parent_of (^^a)) == std::string_view ("foo")); }
  template <int N>
  void bar () { int a; static_assert (identifier_of (parent_of (^^a)) == std::string_view ("bar")); }
};
static_assert (identifier_of (^^V::foo) == std::string_view ("foo"));
static_assert (identifier_of (^^V::bar) == std::string_view ("bar"));

int
operator ""_a (const char *)
{
  int a;
  static_assert (identifier_of (parent_of (^^a)) == std::string_view ("_a"));
  return 0;
}

int v;
static_assert (identifier_of (^^v) == std::string_view ("v"));
template<typename T>
T vtempl;
static_assert (identifier_of (^^vtempl) == std::string_view ("vtempl"));

void foo (int);
static_assert (identifier_of (^^foo) == std::string_view ("foo"));

int arr[3];

void
foo (int a)
{
  auto [b, c, d] = arr;
  static_assert (identifier_of (^^foo) == std::string_view ("foo"));
  static_assert (identifier_of (^^a) == std::string_view ("a"));
  static_assert (identifier_of (^^b) == std::string_view ("b"));
  static_assert (identifier_of (^^c) == std::string_view ("c"));
  static_assert (identifier_of (^^d) == std::string_view ("d"));
}

template <int N>
void
bar (int a)
{
  auto [...b, c] = arr;
  static_assert (identifier_of (^^a) == std::string_view ("a"));
  static_assert (identifier_of (^^c) == std::string_view ("c"));
}
static_assert (identifier_of (^^bar) == std::string_view ("bar"));

void
baz ()
{
  auto a = [] {
    int a;
    static_assert (identifier_of (^^a) == std::string_view ("a"));
    static_assert (identifier_of (parent_of (parent_of (parent_of (^^a)))) == std::string_view ("baz"));
  };
  using t = decltype (a);
  static_assert (identifier_of (^^t) == std::string_view ("t"));
}

void qux (int, int b, int c, int d, int);
constexpr auto p0 = parameters_of (^^qux)[0];
constexpr auto p1 = parameters_of (^^qux)[1];
constexpr auto p2 = parameters_of (^^qux)[2];
constexpr auto p3 = parameters_of (^^qux)[3];
constexpr auto p4 = parameters_of (^^qux)[4];
static_assert (identifier_of (p1) == std::string_view ("b"));
static_assert (identifier_of (p2) == std::string_view ("c"));
static_assert (identifier_of (p3) == std::string_view ("d"));
void qux (int a, int, int c, int e, int);
static_assert (identifier_of (p0) == std::string_view ("a"));
static_assert (identifier_of (p1) == std::string_view ("b"));
static_assert (identifier_of (p2) == std::string_view ("c"));

void
qux (int a, int, int, int e, int)
{
  static_assert (identifier_of (p0) == std::string_view ("a"));
  static_assert (identifier_of (p1) == std::string_view ("b"));
  static_assert (identifier_of (p2) == std::string_view ("c"));
  static_assert (identifier_of (variable_of (p0)) == std::string_view ("a"));
  static_assert (identifier_of (variable_of (p3)) == std::string_view ("e"));
}

void qux (int f, int, int, int, int g);
static_assert (identifier_of (p1) == std::string_view ("b"));
static_assert (identifier_of (p2) == std::string_view ("c"));
static_assert (identifier_of (p4) == std::string_view ("g"));

template <typename... T>
void
freddy (int a, T... b)
{
}

static_assert (identifier_of (parameters_of (^^freddy <int, long, char>)[0]) == std::string_view ("a"));

struct {
  int a;
} s;

static_assert (identifier_of (^^s) == std::string_view ("s"));

typedef struct {
  int b;
} TN;

static_assert (identifier_of (parent_of (^^TN::b)) == std::string_view ("TN"));
static_assert (identifier_of (^^TN) == std::string_view ("TN"));
static_assert (identifier_of (dealias (^^TN)) == std::string_view ("TN"));

typedef enum {
  E8,
  E9
} E10;
static_assert (identifier_of (parent_of (^^E8)) == std::string_view ("E10"));
static_assert (identifier_of (^^E10) == std::string_view ("E10"));
static_assert (identifier_of (dealias (^^E10)) == std::string_view ("E10"));
