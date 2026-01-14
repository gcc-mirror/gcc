// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::bit_size_of.

#include <meta>
#include <climits>

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
has_bit_size_of (info r)
{
  try { bit_size_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (has_bit_size_of (std::meta::reflect_constant (42)));
static_assert (has_bit_size_of (std::meta::reflect_object (arr[1])));
static_assert (has_bit_size_of (^^arr));
static_assert (!has_bit_size_of (^^a3));
static_assert (!has_bit_size_of (^^fn));
static_assert (!has_bit_size_of (^^fn2));
static_assert (!has_bit_size_of (^^Enum::A));
static_assert (has_bit_size_of (^^Alias));
static_assert (has_bit_size_of (^^S));
static_assert (has_bit_size_of (^^S::mem));
static_assert (has_bit_size_of (std::meta::members_of (^^S, ctx)[1]));
static_assert (!has_bit_size_of (^^TCls));
static_assert (!has_bit_size_of (^^TFn));
static_assert (!has_bit_size_of (^^TVar));
static_assert (!has_bit_size_of (^^Concept));
static_assert (!has_bit_size_of (^^NSAlias));
static_assert (!has_bit_size_of (^^NS));
static_assert (has_bit_size_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (has_bit_size_of (std::meta::data_member_spec (^^int, { .name = "member" })));
static_assert (has_bit_size_of (std::meta::data_member_spec (^^int, { .name = "member2", .bit_width = 6 })));
static_assert (!has_bit_size_of (^^arr2));
static_assert (has_bit_size_of (^^arr3));
static_assert (!has_bit_size_of (^^ref));
static_assert (bit_size_of (^^arr) == CHAR_BIT * sizeof (arr));
static_assert (bit_size_of (std::meta::reflect_constant (42)) == CHAR_BIT * sizeof (int));
static_assert (bit_size_of (^^Alias) == CHAR_BIT * sizeof (int));
static_assert (bit_size_of (^^S) == CHAR_BIT * sizeof (S));
static_assert (bit_size_of (^^S::mem) == CHAR_BIT * sizeof (S::mem));
static_assert (bit_size_of (^^arr3) == CHAR_BIT * sizeof (arr3));
using fnt = int (int, int);
static_assert (!has_bit_size_of (^^fnt));
void bar (long, const T f, int g[2], T &);

int
foo (int a, const long b, T c, int d[4], T &e, int f)
{
  static_assert (has_bit_size_of (^^a));
  static_assert (has_bit_size_of (^^b));
  static_assert (has_bit_size_of (^^c));
  static_assert (has_bit_size_of (^^d));
  static_assert (!has_bit_size_of (^^e));
  static_assert (!has_bit_size_of (parameters_of (^^foo)[0]));
  static_assert (!has_bit_size_of (parameters_of (^^foo)[5]));
  static_assert (!has_bit_size_of (parameters_of (^^bar)[0]));
  static_assert (bit_size_of (^^a) == CHAR_BIT * sizeof (int));
  static_assert (bit_size_of (^^b) == CHAR_BIT * sizeof (long));
  static_assert (bit_size_of (^^c) == CHAR_BIT * sizeof (T));
  static_assert (bit_size_of (^^d) == CHAR_BIT * sizeof (int *));
  return 0;
}

struct V
{
  char a;
  long long f;
  int : 2;
  int g : 3;
  int &h;
  long long i : sizeof (long long) * CHAR_BIT - 3;
};
using CV = const V;

static_assert (bit_size_of (^^int) == CHAR_BIT * sizeof (int));
static_assert (bit_size_of (^^char) == CHAR_BIT * sizeof (char));
static_assert (bit_size_of (^^V) == CHAR_BIT * sizeof (V));
static_assert (bit_size_of (^^CV) == CHAR_BIT * sizeof (V));
static_assert (bit_size_of (^^char *) == CHAR_BIT * sizeof (char *));
static_assert (bit_size_of (^^char &) == CHAR_BIT * sizeof (char *));
static_assert (sizeof (char) == sizeof (char *) || bit_size_of (^^char &) != CHAR_BIT * sizeof (char &));
static_assert (bit_size_of (^^V::a) == CHAR_BIT * sizeof (char));
static_assert (bit_size_of (^^V::f) == CHAR_BIT * sizeof (long long));
static_assert (bit_size_of (^^V::g) == 3);
static_assert (bit_size_of (^^V::h) == CHAR_BIT * sizeof (int *));
static_assert (bit_size_of (^^V::i) == CHAR_BIT * sizeof (long long) - 3);
static_assert (bit_size_of (std::meta::bases_of (^^S, ctx)[0]) == CHAR_BIT * sizeof (B));
static_assert (bit_size_of (std::meta::data_member_spec (^^int, { .name = "member" })) == CHAR_BIT * sizeof (int));
static_assert (bit_size_of (std::meta::data_member_spec (^^int, { .name = "member2", .bit_width = 6 })) == 6);
