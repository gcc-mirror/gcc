// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_variable.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
struct cls {
  int dm;
  static int static_dm;
  void mem_fun ();
  static void static_mem_fun ();
  int &ref_dm = dm;
  using type = int;
} cls_var;
union onion { };
static union { int anon; };
using alias = cls;
void fun ();
int var;
int &ref = var;
int &&rref = 42;
int *ptr = &var;
namespace ns {}
namespace ns_alias = ns;
enum Enum { A };
enum class Enum_class { A };

template<typename> struct incomplete_cls;
template<typename> struct cls_tmpl {};
template<typename> void fun_tmpl ();
template<typename> concept conc = requires { true; };
template<typename> int var_tmpl;
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;

int arr[] = { 42 };
auto [ decomp ] = arr;
auto &[ decomp_ref ] = arr;

static_assert (!is_variable (null_reflection));
static_assert (!is_variable (^^::));
static_assert (!is_variable (^^ns));
static_assert (!is_variable (^^ns_alias));
static_assert (!is_variable (reflect_constant (3)));
static_assert (!is_variable (^^cls));
static_assert (!is_variable (^^cls::dm));
static_assert (!is_variable (^^cls::ref_dm));
static_assert (is_variable (^^cls::static_dm));
static_assert (!is_variable (^^cls::mem_fun));
static_assert (!is_variable (^^cls::static_mem_fun));
static_assert (!is_variable (^^cls::type));
static_assert (is_variable (^^cls_var));
static_assert (!is_variable (^^onion));
static_assert (!is_variable (^^anon));
static_assert (!is_variable (^^fun));
static_assert (!is_variable (^^alias));
static_assert (is_variable (^^var));
// ??? EDG says this is _not_ a variable.
static_assert (is_variable (^^ref));
static_assert (is_variable (^^rref));
static_assert (is_variable (^^ptr));
static_assert (!is_variable (^^cls_tmpl));
static_assert (!is_variable (^^cls_tmpl<int>));
static_assert (!is_variable (^^incomplete_cls<int>));
static_assert (!is_variable (^^fun_tmpl));
static_assert (!is_variable (^^fun_tmpl<int>));
static_assert (!is_variable (^^conc));
static_assert (!is_variable (substitute (^^conc, { ^^int })));
static_assert (!is_variable (^^var_tmpl));
static_assert (is_variable (^^var_tmpl<int>));
static_assert (!is_variable (^^cls_tmpl_alias));
static_assert (!is_variable (^^cls_tmpl_alias<int>));
static_assert (!is_variable (^^Enum));
static_assert (!is_variable (^^Enum::A));
static_assert (!is_variable (^^Enum_class));
static_assert (!is_variable (^^Enum_class::A));
static_assert (!is_variable (^^decomp));
static_assert (!is_variable (^^decomp_ref));
static_assert (is_variable (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_variable (dms));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_variable (^^T));
  static_assert (is_variable (R));
  static_assert (!is_variable (R2));
  static_assert (!is_variable (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (is_variable (^^p));
  static_assert (is_variable (^^c));
  static_assert (!is_variable (parameters_of (^^g)[0]));
  static_assert (!is_variable (parameters_of (^^g)[1]));
}
