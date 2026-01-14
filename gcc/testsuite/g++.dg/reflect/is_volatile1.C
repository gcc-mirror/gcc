// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_volatile.

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

static_assert (!is_volatile (std::meta::reflect_constant (42)));
static_assert (!is_volatile (std::meta::reflect_object (arr[1])));
static_assert (!is_volatile (^^arr));
static_assert (!is_volatile (^^a3));
static_assert (!is_volatile (^^fn));
static_assert (!is_volatile (^^fn2));
static_assert (!is_volatile (^^Enum::A));
static_assert (!is_volatile (^^Alias));
static_assert (!is_volatile (^^S));
static_assert (!is_volatile (^^S::mem));
static_assert (!is_volatile (std::meta::members_of (^^S, ctx)[1]));
static_assert (!is_volatile (^^TCls));
static_assert (!is_volatile (^^TFn));
static_assert (!is_volatile (^^TVar));
static_assert (!is_volatile (^^Concept));
static_assert (!is_volatile (^^NSAlias));
static_assert (!is_volatile (^^NS));
static_assert (!is_volatile (std::meta::bases_of (^^S, ctx)[0]));
static_assert (!is_volatile (std::meta::data_member_spec (^^int, { .name = "member" })));

int
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (!is_volatile (^^a));
  static_assert (!is_volatile (^^b));
  static_assert (!is_volatile (^^c));
  static_assert (!is_volatile (^^d));
  static_assert (!is_volatile (^^e));
  static_assert (!is_const (parameters_of (^^foo)[0]));
  static_assert (!is_const (parameters_of (^^foo)[1]));
  static_assert (!is_const (parameters_of (^^foo)[2]));
  static_assert (!is_const (parameters_of (^^foo)[3]));
  static_assert (!is_const (parameters_of (^^foo)[4]));
  return 0;
}

static_assert (!is_volatile (std::meta::reflect_constant (42)));
static_assert (!is_volatile (std::meta::reflect_constant (42.0)));
static_assert (!is_volatile (std::meta::reflect_constant (U { 42 })));
static_assert (!is_volatile (std::meta::reflect_object (arr[1])));
static_assert (!is_volatile (^^arr));
static_assert (!is_volatile (^^fn));
static_assert (!is_volatile (^^Enum::A));
static_assert (!is_volatile (^^A));
static_assert (!is_volatile (^^S::mem));

volatile int vi = 42;
static_assert (is_volatile (^^vi));
using uvi = volatile int;
static_assert (is_volatile (^^uvi));
const volatile long vl = 42L;
static_assert (is_volatile (^^vl));
extern const volatile short cv = 2;
static_assert (is_volatile (^^cv));
struct V { volatile int a; static volatile int b; };
static_assert (is_volatile (^^V::a));
static_assert (is_volatile (^^V::b));

volatile int arr2[] = {3, 4, 5};
volatile int arr3[2][2][2] = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
struct W {
  void foo () volatile {}
  void bar () const {}
};
static_assert (is_volatile (^^arr2));
static_assert (is_volatile (^^arr3));
static_assert (is_volatile (^^W::foo));
static_assert (!is_volatile (^^W::bar));
static_assert (!is_volatile (^^int));
static_assert (!is_volatile (^^const int));
static_assert (is_volatile (^^volatile int));
static_assert (is_volatile (^^const volatile int));
static_assert (!is_volatile (^^int [2]));
static_assert (!is_volatile (^^const int [2]));
static_assert (is_volatile (^^volatile int [2]));
static_assert (is_volatile (^^const volatile int [2]));
static_assert (!is_volatile (^^int (int)));
static_assert (!is_volatile (^^int (int) const));
static_assert (is_volatile (^^int (int) volatile));
static_assert (is_volatile (^^int (int) const volatile));
