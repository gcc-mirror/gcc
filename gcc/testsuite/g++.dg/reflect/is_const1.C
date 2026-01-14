// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_const.

#include <meta>

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

static_assert (!is_const (std::meta::reflect_constant (42)));
static_assert (!is_const (std::meta::reflect_object (arr[1])));
static_assert (!is_const (^^arr));
static_assert (!is_const (^^a3));
static_assert (!is_const (^^fn));
static_assert (!is_const (^^fn2));
static_assert (!is_const (^^Enum::A));
static_assert (!is_const (^^Alias));
static_assert (!is_const (^^S));
static_assert (!is_const (^^S::mem));
static_assert (!is_const (std::meta::members_of (^^S, ctx)[1]));
static_assert (!is_const (^^TCls));
static_assert (!is_const (^^TFn));
static_assert (!is_const (^^TVar));
static_assert (!is_const (^^Concept));
static_assert (!is_const (^^NSAlias));
static_assert (!is_const (^^NS));
static_assert (!is_const (std::meta::bases_of (^^S, ctx)[0]));
static_assert (!is_const (std::meta::data_member_spec (^^int, { .name = "member" })));

int
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (!is_const (^^a));
  static_assert (is_const (^^b));
  static_assert (!is_const (^^c));
  static_assert (!is_const (^^d));
  static_assert (!is_const (^^e));
  static_assert (!is_const (parameters_of (^^foo)[0]));
  static_assert (!is_const (parameters_of (^^foo)[1]));
  static_assert (!is_const (parameters_of (^^foo)[2]));
  static_assert (!is_const (parameters_of (^^foo)[3]));
  static_assert (!is_const (parameters_of (^^foo)[4]));
  return 0;
}

static_assert (!is_const (std::meta::reflect_constant (42)));
static_assert (!is_const (std::meta::reflect_constant (42.0)));
static_assert (is_const (std::meta::reflect_constant (U { 42 })));
static_assert (!is_const (std::meta::reflect_object (arr[1])));
static_assert (!is_const (^^arr));
static_assert (!is_const (^^fn));
static_assert (!is_const (^^Enum::A));
static_assert (!is_const (^^A));
static_assert (!is_const (^^S::mem));

const int ci = 42;
static_assert (is_const (^^ci));
using uci = const int;
static_assert (is_const (^^uci));
constexpr long cl = 42L;
static_assert (is_const (^^cl));
extern const volatile short cv = 2;
static_assert (is_const (^^cv));
struct V { const int a; static const int b; };
static_assert (is_const (^^V::a));
static_assert (is_const (^^V::b));

const int arr2[] = {3, 4, 5};
const int arr3[2][2][2] = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
struct W {
  void foo () volatile {}
  void bar () const {}
};
static_assert (is_const (^^arr2));
static_assert (is_const (^^arr3));
static_assert (!is_const (^^W::foo));
static_assert (is_const (^^W::bar));
static_assert (!is_const (^^int));
static_assert (is_const (^^const int));
static_assert (!is_const (^^volatile int));
static_assert (is_const (^^const volatile int));
static_assert (!is_const (^^int [2]));
static_assert (is_const (^^const int [2]));
static_assert (!is_const (^^volatile int [2]));
static_assert (is_const (^^const volatile int [2]));
static_assert (!is_const (^^int (int)));
static_assert (is_const (^^int (int) const));
static_assert (!is_const (^^int (int) volatile));
static_assert (is_const (^^int (int) const volatile));
