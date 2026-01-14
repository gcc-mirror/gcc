// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::offset_of.

#include <meta>
#include <cstddef>

using namespace std::meta;

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
void fn();
auto &fn2();
enum Enum { A };
using Alias = int;
struct B {};
struct S : B {
  int mem;
  int : 0;
};
struct T {
  T ();
  T (const T &);
  ~T ();
};
struct U {
  int u;
};
template<auto> struct TCls {};
template<auto> void TFn();
template<auto> int TVar;
template<auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;

constexpr auto ctx = std::meta::access_context::current ();

consteval bool
has_offset_of (info r)
{
  try { offset_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_offset_of (std::meta::reflect_constant (42)));
static_assert (!has_offset_of (std::meta::reflect_object (arr[1])));
static_assert (!has_offset_of (^^arr));
static_assert (!has_offset_of (^^a3));
static_assert (!has_offset_of (^^fn));
static_assert (!has_offset_of (^^fn2));
static_assert (!has_offset_of (^^Enum::A));
static_assert (!has_offset_of (^^Alias));
static_assert (!has_offset_of (^^S));
static_assert (has_offset_of (^^S::mem));
static_assert (has_offset_of (std::meta::members_of (^^S, ctx)[1]));
static_assert (!has_offset_of (^^TCls));
static_assert (!has_offset_of (^^TFn));
static_assert (!has_offset_of (^^TVar));
static_assert (!has_offset_of (^^Concept));
static_assert (!has_offset_of (^^NSAlias));
static_assert (!has_offset_of (^^NS));
static_assert (has_offset_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (!has_offset_of (std::meta::data_member_spec (^^int, { .name = "member" })));
void bar (long, const T f, int g[2], T &);

int
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (!has_offset_of (^^a));
  static_assert (!has_offset_of (^^b));
  static_assert (!has_offset_of (^^c));
  static_assert (!has_offset_of (^^d));
  static_assert (!has_offset_of (^^e));
  static_assert (!has_offset_of (parameters_of (^^foo)[0]));
  static_assert (!has_offset_of (parameters_of (^^bar)[0]));
  return 0;
}

struct V
{
  char a;
  short b;
  char c;
  int d;
  char e;
  long long f;
  int : 2;
  int g : 3;
  union {
    int h;
    long i;
  };
};

union W
{
  int a;
  double b;
};

static_assert (offset_of (^^V::a).bytes == offsetof (V, a));
static_assert (offset_of (^^V::a).bits == 0);
static_assert (offset_of (^^V::b).bytes == offsetof (V, b));
static_assert (offset_of (^^V::b).bits == 0);
static_assert (offset_of (^^V::c).bytes == offsetof (V, c));
static_assert (offset_of (^^V::c).bits == 0);
static_assert (offset_of (^^V::d).bytes == offsetof (V, d));
static_assert (offset_of (^^V::d).bits == 0);
static_assert (offset_of (^^V::e).bytes == offsetof (V, e));
static_assert (offset_of (^^V::e).bits == 0);
static_assert (offset_of (^^V::f).bytes == offsetof (V, f));
static_assert (offset_of (^^V::f).bits == 0);
static_assert (offset_of (^^V::g).bytes == offsetof (V, f) + sizeof (long long));
static_assert (offset_of (^^V::g).bits == 2);
static_assert (offset_of (^^V::h).bytes == 0);
static_assert (offset_of (^^V::h).bits == 0);
static_assert (offset_of (^^V::i).bytes == 0);
static_assert (offset_of (^^V::i).bits == 0);
static_assert (offset_of (^^W::a).bytes == 0);
static_assert (offset_of (^^W::a).bits == 0);
static_assert (offset_of (^^W::b).bytes == 0);
static_assert (offset_of (^^W::b).bits == 0);
static_assert (offset_of (^^V::f) < offset_of (^^V::g));
static_assert (offset_of (^^V::f) > offset_of (^^V::e));
static_assert (offset_of (^^V::g) == offset_of (^^V::g));
static_assert (offset_of (^^V::e).total_bits () == offsetof (V, e) * __CHAR_BIT__);
static_assert (offset_of (^^V::g).total_bits () == (offsetof (V, f) + sizeof (long long)) * __CHAR_BIT__ + 2);
using mo = decltype (offset_of (^^V::g));
static_assert (dealias (^^mo) == ^^std::meta::member_offset);
constexpr std::meta::member_offset mov = { .bytes = 1, .bits = 2 };
static_assert (mov.total_bits () == __CHAR_BIT__ + 2);
