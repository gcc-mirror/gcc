// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::alignment_of.

#include <meta>
#include <cstddef>

using namespace std::meta;

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
extern int arr2[];
extern int arr3[2];
alignas (32) int arr4[] = {1, 2, 3};
alignas (64) extern int arr5[2];
void fn();
auto &fn2();
enum Enum { A };
using Alias = int;
struct B {};
struct S : B {
  int mem;
  int : 0;
} s;
struct S2 {
  char p;
  int mem;
};
struct T {
  T ();
  T (const T &);
  ~T ();
  static int ta;
  alignas (128) static constexpr int tb = 42;
};
int T::ta;
struct U {
  int u;
};
static short v1;
alignas (64) static long long v2;
template<auto> struct TCls {};
template<auto> void TFn();
template<auto> int TVar;
template<auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;
int &ref = arr[0];

constexpr auto ctx = std::meta::access_context::current ();

consteval bool
has_alignment_of (info r)
{
  try { alignment_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_alignment_of (std::meta::reflect_constant (42)));
static_assert (has_alignment_of (std::meta::reflect_object (arr)));
static_assert (has_alignment_of (std::meta::reflect_object (arr[1])));
static_assert (has_alignment_of (std::meta::reflect_object (s.mem)));
static_assert (has_alignment_of (^^arr));
static_assert (!has_alignment_of (^^a3));
static_assert (!has_alignment_of (^^fn));
static_assert (!has_alignment_of (^^fn2));
static_assert (!has_alignment_of (^^Enum::A));
static_assert (has_alignment_of (^^Alias));
static_assert (has_alignment_of (^^S));
static_assert (has_alignment_of (^^S::mem));
static_assert (!has_alignment_of (members_of (^^S, access_context::current ())[1]));
static_assert (!has_alignment_of (^^TCls));
static_assert (!has_alignment_of (^^TFn));
static_assert (!has_alignment_of (^^TVar));
static_assert (!has_alignment_of (^^Concept));
static_assert (!has_alignment_of (^^NSAlias));
static_assert (!has_alignment_of (^^NS));
static_assert (has_alignment_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (has_alignment_of (std::meta::data_member_spec (^^int, { .name = "member" })));
static_assert (!has_alignment_of (std::meta::data_member_spec (^^int, { .name = "member", .bit_width = 6 })));
static_assert (!has_alignment_of (std::meta::data_member_spec (^^int, { .bit_width = 15 })));
static_assert (has_alignment_of (^^arr2));
static_assert (has_alignment_of (^^arr3));
static_assert (!has_alignment_of (^^ref));
static_assert (alignment_of (std::meta::reflect_object (arr)) == alignment_of (^^arr));
static_assert (alignment_of (std::meta::reflect_object (arr[1])) == alignof (int));
static_assert (alignment_of (std::meta::reflect_object (s.mem)) == alignment_of (^^S::mem));
static_assert (alignment_of (^^arr) >= alignof (int));
static_assert (alignment_of (^^arr4) == 32);
static_assert (alignment_of (^^Alias) == alignof (int));
static_assert (alignment_of (^^S) == alignof (S));
static_assert (alignment_of (^^S::mem) == offsetof (S2, mem));
static_assert (alignment_of (std::meta::bases_of (^^S, ctx)[0]) == alignof (B));
static_assert (alignment_of (std::meta::data_member_spec (^^int, { .name = "member" })) == alignof (int));
static_assert (alignment_of (^^arr3) >= alignof (int));
static_assert (alignment_of (^^arr5) == 64);
static_assert (alignment_of (^^T::ta) >= alignof (int));
static_assert (alignment_of (^^T::tb) == 128);
static_assert (alignment_of (^^v1) >= alignof (short));
static_assert (alignment_of (^^v2) == 64);
using fnt = int (int, int);
static_assert (!has_alignment_of (^^fnt));
void bar (long, const T f, int g[2], T &);

int
foo (int a, const long b, T c, int d[4], T &e, int f)
{
  static_assert (has_alignment_of (^^a));
  static_assert (has_alignment_of (^^b));
  static_assert (has_alignment_of (^^c));
  static_assert (has_alignment_of (^^d));
  static_assert (!has_alignment_of (^^e));
  static_assert (!has_alignment_of (parameters_of (^^foo)[0]));
  static_assert (!has_alignment_of (parameters_of (^^foo)[5]));
  static_assert (!has_alignment_of (parameters_of (^^bar)[0]));
  static_assert (alignment_of (^^a) >= alignof (int));
  static_assert (alignment_of (^^b) >= alignof (long));
  static_assert (alignment_of (^^c) >= alignof (T));
  static_assert (alignment_of (^^d) >= alignof (int *));
  return 0;
}

struct V
{
  char a;
  long long f;
  int : 2;
  int g : 3;
  int &h;
  alignas (32) int i;
  alignas (64) long long j;
};
struct V2
{
  char p;
  char a;
};
struct V3
{
  char p;
  long long f;
};
struct V4
{
  char p;
  int *h;
};
using CV = const V;

static_assert (alignment_of (^^int) == alignof (int));
static_assert (alignment_of (^^char) == alignof (char));
static_assert (alignment_of (^^V) == alignof (V));
static_assert (alignment_of (^^CV) == alignof (V));
static_assert (alignment_of (^^char *) == alignof (char *));
static_assert (alignment_of (^^char &) == alignof (char *));
static_assert (alignment_of (^^char &&) == alignof (char *));
static_assert (alignof (char) == alignof (char *) || alignment_of (^^char &) != alignof (char &));
static_assert (alignof (char) == alignof (char *) || alignment_of (^^char &&) != alignof (char &&));
static_assert (alignment_of (^^V::a) == offsetof (V2, a));
static_assert (alignment_of (^^V::f) == offsetof (V3, f));
static_assert (!has_alignment_of (^^V::g));
static_assert (alignment_of (^^V::h) == offsetof (V4, h));
static_assert (alignment_of (^^V::i) == 32);
static_assert (alignment_of (^^V::j) == 64);
