// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::variable_of.

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

consteval bool
has_variable_of (info r)
{
  try { variable_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_variable_of (std::meta::reflect_constant (42)));
static_assert (!has_variable_of (std::meta::reflect_object (arr[1])));
static_assert (!has_variable_of (^^arr));
static_assert (!has_variable_of (^^a3));
static_assert (!has_variable_of (^^fn));
static_assert (!has_variable_of (^^fn2));
static_assert (!has_variable_of (^^Enum::A));
static_assert (!has_variable_of (^^Alias));
static_assert (!has_variable_of (^^S));
static_assert (!has_variable_of (^^S::mem));
static_assert (!has_variable_of (std::meta::members_of (^^S, ctx)[1]));
static_assert (!has_variable_of (^^TCls));
static_assert (!has_variable_of (^^TFn));
static_assert (!has_variable_of (^^TVar));
static_assert (!has_variable_of (^^Concept));
static_assert (!has_variable_of (^^NSAlias));
static_assert (!has_variable_of (^^NS));
static_assert (!has_variable_of (std::meta::bases_of (^^S, ctx)[0]));
static_assert (!has_variable_of (std::meta::data_member_spec (^^int, { .name = "member" })));

void
bar (long, const T f, int g[2], T &)
{
}

void
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (!has_variable_of (^^a));
  static_assert (!has_variable_of (^^b));
  static_assert (!has_variable_of (^^c));
  static_assert (!has_variable_of (^^d));
  static_assert (!has_variable_of (^^e));
  static_assert (has_variable_of (parameters_of (^^foo)[0]));
  static_assert (has_variable_of (parameters_of (^^foo)[1]));
  static_assert (has_variable_of (parameters_of (^^foo)[2]));
  static_assert (has_variable_of (parameters_of (^^foo)[3]));
  static_assert (has_variable_of (parameters_of (^^foo)[4]));
  static_assert (!has_variable_of (parameters_of (^^bar)[0]));
  static_assert (!has_variable_of (parameters_of (^^bar)[1]));
  static_assert (!has_variable_of (parameters_of (^^bar)[2]));
  static_assert (!has_variable_of (parameters_of (^^bar)[3]));
  static_assert (variable_of (parameters_of (^^foo)[0]) == ^^a);
  static_assert (variable_of (parameters_of (^^foo)[1]) == ^^b);
  static_assert (variable_of (parameters_of (^^foo)[2]) == ^^c);
  static_assert (variable_of (parameters_of (^^foo)[3]) == ^^d);
  static_assert (variable_of (parameters_of (^^foo)[4]) == ^^e);
  static_assert (variable_of (parameters_of (^^foo)[0]) != parameters_of (^^foo)[0]);
  static_assert (variable_of (parameters_of (^^foo)[1]) != parameters_of (^^foo)[1]);
  static_assert (variable_of (parameters_of (^^foo)[2]) != parameters_of (^^foo)[2]);
  static_assert (variable_of (parameters_of (^^foo)[3]) != parameters_of (^^foo)[3]);
  static_assert (variable_of (parameters_of (^^foo)[4]) != parameters_of (^^foo)[4]);
  static_assert (parameters_of (^^foo)[0] != ^^a);
  static_assert (parameters_of (^^foo)[1] != ^^b);
  static_assert (parameters_of (^^foo)[2] != ^^c);
  static_assert (parameters_of (^^foo)[3] != ^^d);
  static_assert (parameters_of (^^foo)[4] != ^^e);
}

consteval bool
baz (int a, info b, info c, info d)
{
  if (a != 43)
    return false;
  if (b != d)
    return false;
  if (b == c)
    return false;
  if (b == parameters_of (^^baz)[1])
    return false;

  // variable can be accessed only in innermost scope
  try { variable_of (c); return false; }
  catch (exception) {}
  try { variable_of (parameters_of (^^baz)[1]); return false; } 
  catch (exception) {}
  return true;
}

consteval bool
qux (int a)
{
  return baz (a + 1, ^^a, parameters_of (^^qux)[0],
  // TODO: This doesn't work yet, I think we want to check
  // whether DECL_CONTEXT of the PARM_DECL is current_function_decl
  // or any of the ctx->call->fundef->decl on the constexpr evaluation
  // stack.  But currently we only have access to the innermost.
	      // variable_of (parameters_of (^^qux)[0]));
	      ^^a);
}

static_assert (qux (42));
