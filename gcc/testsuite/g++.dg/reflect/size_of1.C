// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::size_of.

#include <meta>

using namespace std::meta;

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
extern int arr2[];
extern int arr3[2];
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
int &ref = arr[0];

constexpr auto ctx = std::meta::access_context::current ();

consteval bool
has_size_of (info r)
{
  try { size_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (has_size_of (std::meta::reflect_constant (42)));
static_assert (has_size_of (std::meta::reflect_object (arr[1])));
static_assert (has_size_of (^^arr));
static_assert (!has_size_of (^^a3));
static_assert (!has_size_of (^^fn));
static_assert (!has_size_of (^^fn2));
static_assert (!has_size_of (^^Enum::A));
static_assert (has_size_of (^^Alias));
static_assert (has_size_of (^^S));
static_assert (has_size_of (^^S::mem));
static_assert (!has_size_of (members_of (^^S, access_context::current ())[1]));
static_assert (!has_size_of (^^TCls));
static_assert (!has_size_of (^^TFn));
static_assert (!has_size_of (^^TVar));
static_assert (!has_size_of (^^Concept));
static_assert (!has_size_of (^^NSAlias));
static_assert (!has_size_of (^^NS));
static_assert (has_size_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (has_size_of (std::meta::data_member_spec (^^int, { .name = "member" })));
static_assert (!has_size_of (std::meta::data_member_spec (^^int, { .name = "member", .bit_width = 6 })));
static_assert (!has_size_of (std::meta::data_member_spec (^^int, { .bit_width = 15 })));
static_assert (!has_size_of (^^arr2));
static_assert (has_size_of (^^arr3));
static_assert (!has_size_of (^^ref));
static_assert (size_of (^^arr) == sizeof (arr));
static_assert (size_of (std::meta::reflect_constant (42)) == sizeof (int));
static_assert (size_of (^^Alias) == sizeof (int));
static_assert (size_of (^^S) == sizeof (S));
static_assert (size_of (^^S::mem) == sizeof (S::mem));
static_assert (size_of (std::meta::bases_of (^^S, ctx)[0]) == sizeof (B));
static_assert (size_of (std::meta::data_member_spec (^^int, { .name = "member" })) == sizeof (int));
static_assert (size_of (^^arr3) == sizeof (arr3));
using fnt = int (int, int);
static_assert (!has_size_of (^^fnt));
void bar (long, const T f, int g[2], T &);

int
foo (int a, const long b, T c, int d[4], T &e, int f)
{
  static_assert (has_size_of (^^a));
  static_assert (has_size_of (^^b));
  static_assert (has_size_of (^^c));
  static_assert (has_size_of (^^d));
  static_assert (!has_size_of (^^e));
  static_assert (!has_size_of (parameters_of (^^foo)[0]));
  static_assert (!has_size_of (parameters_of (^^foo)[5]));
  static_assert (!has_size_of (parameters_of (^^bar)[0]));
  static_assert (size_of (^^a) == sizeof (int));
  static_assert (size_of (^^b) == sizeof (long));
  static_assert (size_of (^^c) == sizeof (T));
  static_assert (size_of (^^d) == sizeof (int *));
  return 0;
}

struct V
{
  char a;
  long long f;
  int : 2;
  int g : 3;
  int &h;
};
using CV = const V;

static_assert (size_of (^^int) == sizeof (int));
static_assert (size_of (^^char) == sizeof (char));
static_assert (size_of (^^V) == sizeof (V));
static_assert (size_of (^^CV) == sizeof (V));
static_assert (size_of (^^char *) == sizeof (char *));
static_assert (size_of (^^char &) == sizeof (char *));
static_assert (sizeof (char) == sizeof (char *) || size_of (^^char &) != sizeof (char &));
static_assert (size_of (^^V::a) == sizeof (char));
static_assert (size_of (^^V::f) == sizeof (long long));
static_assert (!has_size_of (^^V::g));
static_assert (size_of (^^V::h) == sizeof (int *));
