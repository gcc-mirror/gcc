// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_identifier.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;

struct S { };
using T = int;
using U = S;
enum E { E1, E2 };
enum { E3, E4 };
typedef enum { E5, E6 } E7;
union P { int a; };

static_assert (!has_identifier (null_reflection));
static_assert (!has_identifier (^^int));
static_assert (has_identifier (^^T));
static_assert (!has_identifier (^^::));
static_assert (has_identifier (^^S));
static_assert (has_identifier (^^U));
static_assert (has_identifier (^^P));
static_assert (!has_identifier (^^const S));
static_assert (!has_identifier (^^const volatile U));
static_assert (!has_identifier (^^P volatile));
static_assert (has_identifier (^^std));
static_assert (has_identifier (^^std::meta));
static_assert (!has_identifier (^^int *));
static_assert (!has_identifier (^^int));
static_assert (!has_identifier (^^unsigned long long));
static_assert (!has_identifier (^^long &&));
static_assert (has_identifier (^^E1));
static_assert (has_identifier (^^E));
static_assert (!has_identifier (^^E const));
static_assert (!has_identifier (^^volatile E));
static_assert (!has_identifier (^^const E volatile));
static_assert (!has_identifier (parent_of (^^E3)));
static_assert (has_identifier (parent_of (^^E5)));
static_assert (has_identifier (^^E7));
static_assert (has_identifier (dealias (^^E7)));
[[=1]] int w;
static_assert (!has_identifier (annotations_of (^^w)[0]));
static_assert (!has_identifier (data_member_spec (^^int, { .bit_width = 0 })));
static_assert (!has_identifier (data_member_spec (^^long, { .bit_width = 6 })));
static_assert (has_identifier (data_member_spec (^^long, { .name = "dms", .bit_width = 6 })));
static_assert (has_identifier (data_member_spec (^^long, { .name = u8"dms", .alignment = 2 * alignof (long) })));

namespace N {}
namespace NA = N;
static_assert (has_identifier (^^N));
static_assert (has_identifier (^^NA));

namespace {
  int a;
  namespace M {}
  static_assert (has_identifier (^^a));
  static_assert (!has_identifier (parent_of (^^a)));
  static_assert (has_identifier (^^M));
  static_assert (!has_identifier (parent_of (^^M)));
  static_assert (!has_identifier (parent_of (parent_of (^^M))));
}

typedef struct {
  int a;
  static_assert (has_identifier (^^a));
  static_assert (!has_identifier (parent_of (^^a)));
} SV;
static_assert (has_identifier (^^SV));
static_assert (has_identifier (parent_of (^^SV::a)));
static_assert (has_identifier (dealias (^^SV)));

template <int N>
struct ST
{
  static_assert (has_identifier (^^ST));
  static_assert (!has_identifier (^^ST <N>));
};

struct V
{
  V () { int a; static_assert (!has_identifier (parent_of (^^a))); }
  V (int, long) { int a; static_assert (!has_identifier (parent_of (^^a))); }
  template <typename T>
  V (long, T &) { int a; static_assert (!has_identifier (parent_of (^^a))); }
  ~V () { int a; static_assert (!has_identifier (parent_of (^^a))); }
  V &operator = (const V &) { int a; static_assert (!has_identifier (parent_of (^^a))); return *this; }
  V &operator + (const V &) { int a; static_assert (!has_identifier (parent_of (^^a))); return *this; }
  template <typename T>
  V &operator * (const T &) { int a; static_assert (!has_identifier (parent_of (^^a))); return *this; }
  operator int () { int a; static_assert (!has_identifier (parent_of (^^a))); return *this; }
  void foo () { int a; static_assert (has_identifier (parent_of (^^a))); }
  template <int N>
  void bar () { int a; static_assert (has_identifier (parent_of (^^a))); }
};

struct W
{
  template <typename T>
  explicit operator T () { int a; static_assert (!has_identifier (parent_of (^^a))); }
};

int
operator ""_a (const char *)
{
  int a;
  static_assert (has_identifier (parent_of (^^a)));
  return 0;
}

int v;
static_assert (!has_identifier (^^V::operator =));
static_assert (!has_identifier (^^V::operator +));
static_assert (!has_identifier (^^V::operator *));
static_assert (!has_identifier (^^V::operator *<int>));
static_assert (!has_identifier (^^V::operator int));
static_assert (has_identifier (^^V::foo));
static_assert (has_identifier (^^V::bar));
static_assert (!has_identifier (^^V::bar <0>));

void foo (int);
static_assert (has_identifier (^^foo));

int arr[3];

void
foo (int a)
{
  auto [b, c, d] = arr;
  static_assert (has_identifier (^^foo));
  static_assert (has_identifier (^^a));
  static_assert (has_identifier (^^b));
  static_assert (has_identifier (^^c));
  static_assert (has_identifier (^^d));
}

template <int N>
void
bar (int a)
{
  auto [...b, c] = arr;
  static_assert (has_identifier (^^foo));
  static_assert (has_identifier (^^a));
  static_assert (has_identifier (^^c));
}

void
baz ()
{
  auto a = [] {
    int a;
    static_assert (has_identifier (^^a));
    static_assert (!has_identifier (parent_of (^^a)));
    static_assert (!has_identifier (parent_of (parent_of (^^a))));
    static_assert (has_identifier (parent_of (parent_of (parent_of (^^a)))));
  };
  using t = decltype (a);
  static_assert (has_identifier (^^t));
  static_assert (!has_identifier (dealias (^^t)));
}

void qux (int, int b, int c, int d, int);
constexpr auto p0 = parameters_of (^^qux)[0];
constexpr auto p1 = parameters_of (^^qux)[1];
constexpr auto p2 = parameters_of (^^qux)[2];
constexpr auto p3 = parameters_of (^^qux)[3];
constexpr auto p4 = parameters_of (^^qux)[4];
static_assert (!has_identifier (p0));
static_assert (has_identifier (p1));
static_assert (has_identifier (p2));
static_assert (has_identifier (p3));
static_assert (!has_identifier (p4));
void qux (int a, int, int c, int e, int);
static_assert (has_identifier (p0));
static_assert (has_identifier (p1));
static_assert (has_identifier (p2));
static_assert (!has_identifier (p3));
static_assert (!has_identifier (p4));

void
qux (int a, int, int, int e, int)
{
  static_assert (has_identifier (p0));
  static_assert (has_identifier (p1));
  static_assert (has_identifier (p2));
  static_assert (!has_identifier (p3));
  static_assert (!has_identifier (p4));
  static_assert (has_identifier (variable_of (p0)));
  static_assert (!has_identifier (variable_of (p1)));
  static_assert (!has_identifier (variable_of (p2)));
  static_assert (has_identifier (variable_of (p3)));
  static_assert (!has_identifier (variable_of (p4)));
}

void qux (int f, int, int, int, int g);
static_assert (!has_identifier (p0));
static_assert (has_identifier (p1));
static_assert (has_identifier (p2));
static_assert (!has_identifier (p3));
static_assert (has_identifier (p4));

template <typename... T>
void
freddy (int a, T... b)
{
}

static_assert (has_identifier (parameters_of (^^freddy <int, long, char>)[0]));
static_assert (!has_identifier (parameters_of (^^freddy <int, long, char>)[1]));

struct {
  int a;
} s;

static_assert (has_identifier (^^s));
static_assert (!has_identifier (type_of (^^s)));

void
corge ()
{
  __extension__ constexpr bool b = has_identifier (({ struct S2 { }; ^^S2; }));
}

typedef struct {
  int b;
} TN;

static_assert (has_identifier (parent_of (^^TN::b)));
static_assert (has_identifier (^^TN));
static_assert (has_identifier (dealias (^^TN)));

typedef enum {
  E8,
  E9 = has_identifier (parent_of (^^E8)) ? 2 : 3,
} E10;
static_assert (E9 == 3);
static_assert (has_identifier (parent_of (^^E8)));
static_assert (has_identifier (^^E10));
static_assert (has_identifier (dealias (^^E10)));
