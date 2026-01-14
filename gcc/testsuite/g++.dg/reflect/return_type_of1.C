// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::return_type_of.

#include <meta>

using namespace std::meta;

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
void fn();
int &fn2();
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
U fn3 (int);
using fn4 = unsigned long (int, long);
using fn5 = U (...);
auto fn6 (int);
auto &fn7 (long long);

constexpr auto ctx = std::meta::access_context::current ();

consteval bool
has_return_type_of (info r)
{
  try { return_type_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_return_type_of (std::meta::reflect_constant (42)));
static_assert (!has_return_type_of (std::meta::reflect_object (arr[1])));
static_assert (!has_return_type_of (^^arr));
static_assert (!has_return_type_of (^^a3));
static_assert (has_return_type_of (^^fn));
static_assert (has_return_type_of (^^fn2));
static_assert (has_return_type_of (^^fn3));
static_assert (has_return_type_of (^^fn4));
static_assert (has_return_type_of (^^fn5));
static_assert (!has_return_type_of (^^fn6));
static_assert (!has_return_type_of (^^fn7));
using intref = int &;
static_assert (return_type_of (^^fn) == ^^void);
static_assert (return_type_of (^^fn2) == dealias (^^intref));
using ulong = unsigned long;
static_assert (return_type_of (^^fn3) == ^^U);
static_assert (return_type_of (^^fn4) == dealias (^^ulong));
static_assert (return_type_of (^^fn5) == ^^U);
static_assert (!has_return_type_of (^^Enum::A));
static_assert (!has_return_type_of (^^Alias));
static_assert (!has_return_type_of (^^S));
static_assert (!has_return_type_of (^^S::mem));
static_assert (!has_return_type_of (members_of (^^S, ctx)[1]));
static_assert (!has_return_type_of (^^TCls));
static_assert (!has_return_type_of (^^TFn));
static_assert (!has_return_type_of (^^TVar));
static_assert (!has_return_type_of (^^Concept));
static_assert (!has_return_type_of (^^NSAlias));
static_assert (!has_return_type_of (^^NS));
static_assert (!has_return_type_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (!has_return_type_of (std::meta::data_member_spec (^^int, { .name = "member" })));

struct V
{
  V ()
  {
    int i = 42;
    static_assert (!has_return_type_of (parent_of (^^i)));
  }
  ~V ()
  {
    int i = 42;
    static_assert (!has_return_type_of (parent_of (^^i)));
  }
  V (int, long)
  {
    int i = 42;
    static_assert (!has_return_type_of (parent_of (^^i)));
  }
};
