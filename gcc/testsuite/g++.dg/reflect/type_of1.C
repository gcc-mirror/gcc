// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::type_of.

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
  int v : 5;
};
template<auto> struct TCls {};
template<auto> void TFn();
template<auto> int TVar;
template<auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;

constexpr auto ctx = std::meta::access_context::current ();

consteval bool
has_type (info r)
{
  try { type_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (has_type (std::meta::reflect_constant (42)));
static_assert (has_type (std::meta::reflect_object (arr[1])));
static_assert (has_type (^^arr));
static_assert (!has_type (^^a3));
static_assert (has_type (^^fn));
static_assert (!has_type (^^fn2));
static_assert (has_type (^^Enum::A));
static_assert (!has_type (^^Alias));
static_assert (!has_type (^^S));
static_assert (has_type (^^S::mem));
static_assert (has_type (std::meta::members_of (^^S, ctx)[1]));
static_assert (!has_type (^^TCls));
static_assert (!has_type (^^TFn));
static_assert (!has_type (^^TVar));
static_assert (!has_type (^^Concept));
static_assert (!has_type (^^NSAlias));
static_assert (!has_type (^^NS));
static_assert (has_type (std::meta::bases_of (^^S, ctx)[0]));
static_assert (has_type (std::meta::data_member_spec (^^int, { .name = "member" })));
static_assert (has_type (std::meta::data_member_spec (^^int, { .name = "member", .bit_width = 6 })));
static_assert (has_type (std::meta::data_member_spec (^^int, { .bit_width = 0 })));
static_assert (has_type (std::meta::data_member_spec (^^int, { .bit_width = 5 })));
void bar (long, const T f, int g[2], T &);

int
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (has_type (^^a));
  static_assert (has_type (^^b));
  static_assert (has_type (^^c));
  static_assert (has_type (^^d));
  static_assert (has_type (^^e));
  static_assert (has_type (parameters_of (^^foo)[0]));
  static_assert (has_type (parameters_of (^^foo)[1]));
  static_assert (has_type (parameters_of (^^foo)[2]));
  static_assert (has_type (parameters_of (^^foo)[3]));
  static_assert (has_type (parameters_of (^^foo)[4]));
  static_assert (has_type (parameters_of (^^bar)[0]));
  static_assert (has_type (parameters_of (^^bar)[1]));
  static_assert (has_type (parameters_of (^^bar)[2]));
  static_assert (has_type (parameters_of (^^bar)[3]));
  static_assert (type_of (^^a) == ^^int);
  using clong = const long;
  static_assert (type_of (^^b) == dealias(^^clong));
  static_assert (type_of (^^c) == ^^T);
  using ptr = int *;
  static_assert (type_of (^^d) == dealias (^^ptr));
  using ref = T &;
  static_assert (type_of (^^e) == dealias (^^ref));
  static_assert (type_of (parameters_of (^^foo)[0]) == ^^int);
  static_assert (type_of (parameters_of (^^foo)[1]) == ^^long);
  static_assert (type_of (parameters_of (^^foo)[2]) == ^^T);
  static_assert (type_of (parameters_of (^^foo)[3]) == dealias (^^ptr));
  static_assert (type_of (parameters_of (^^foo)[4]) == dealias (^^ref));
  static_assert (type_of (parameters_of (^^bar)[0]) == ^^long);
  static_assert (type_of (parameters_of (^^bar)[1]) == ^^T);
  static_assert (type_of (parameters_of (^^bar)[2]) == dealias (^^ptr));
  static_assert (type_of (parameters_of (^^bar)[3]) == dealias (^^ref));
  return 0;
}

static_assert (type_of (std::meta::reflect_constant (42)) == ^^int);
static_assert (type_of (std::meta::reflect_constant (42.0)) == ^^double);
static_assert (type_of (std::meta::reflect_constant (U { 42 })) == ^^const U);
static_assert (type_of (std::meta::reflect_object (arr[1])) == ^^int);
using int3 = int[3];
static_assert (type_of (^^arr) == dealias (^^int3));
static_assert (type_of (^^fn) == ^^void ());
static_assert (type_of (^^Enum::A) == ^^Enum);
static_assert (type_of (^^A) == ^^Enum);
static_assert (type_of (^^S::mem) == ^^int);
static_assert (type_of (^^U::v) == ^^int);
static_assert (type_of (std::meta::members_of (^^S, ctx)[1]) == ^^int);
static_assert (type_of (std::meta::bases_of (^^S, ctx)[0]) == ^^B);
static_assert (type_of (std::meta::data_member_spec (^^int, { .name = "member" })) == ^^int);
static_assert (type_of (std::meta::data_member_spec (^^const long, { .name = "member", .bit_width = 8 })) == ^^const long);
static_assert (type_of (std::meta::data_member_spec (^^int3, { .name = "member" })) == ^^int[3]);
static_assert (type_of (std::meta::data_member_spec (^^int, { .bit_width = 7 })) == ^^int);
static_assert (type_of (std::meta::data_member_spec (^^int, { .bit_width = 0 })) == ^^int);

consteval int
test (info x, info y)
{
  if (x == y)
    return 0;
  throw 1;
}

static_assert (type_of (^^test) == ^^int (info, info));

using ull = unsigned long long;

enum Enum2 {
  E21,
  E22,
  E23 = 3L,
  E24,
  E25 = 5ULL,
  E26,
  E27 = 7 + test (type_of (^^E21), ^^int)
	  + test (type_of (^^E22), ^^int)
	  + test (type_of (^^E23), ^^long)
	  + test (type_of (^^E24), ^^long)
	  + test (type_of (^^E25), dealias (^^ull))
	  + test (type_of (^^E26), dealias (^^ull))
};

static_assert (type_of (^^E21) == ^^Enum2);
static_assert (type_of (^^E22) == ^^Enum2);
static_assert (type_of (^^E23) == ^^Enum2);
static_assert (type_of (^^E24) == ^^Enum2);
static_assert (type_of (^^E25) == ^^Enum2);
static_assert (type_of (^^E26) == ^^Enum2);
static_assert (type_of (^^E27) == ^^Enum2);

enum Enum3 : long {
  E31,
  E32,
  E33 = 3L,
  E34,
  E35 = 5ULL,
  E36,
  E37 = 7 + test (type_of (^^E31), ^^long)
	  + test (type_of (^^E32), ^^long)
	  + test (type_of (^^E33), ^^long)
	  + test (type_of (^^E34), ^^long)
	  + test (type_of (^^E35), ^^long)
	  + test (type_of (^^E36), ^^long)
};

static_assert (type_of (^^E31) == ^^Enum3);
static_assert (type_of (^^E32) == ^^Enum3);
static_assert (type_of (^^E33) == ^^Enum3);
static_assert (type_of (^^E34) == ^^Enum3);
static_assert (type_of (^^E35) == ^^Enum3);
static_assert (type_of (^^E36) == ^^Enum3);
static_assert (type_of (^^E37) == ^^Enum3);

constexpr auto a = reflect_constant_string ("abcd");
static_assert (type_of (a) == ^^const char [5]);
auto as = &[: a :];
static_assert (type_of (^^as) == ^^const char (*) [5]);
struct V { int a, b, c; };
auto bs = &[: reflect_constant (V { 2, 3, 4 }) :];
static_assert (type_of (^^bs) == ^^const V *);
