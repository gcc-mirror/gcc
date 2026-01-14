// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function_parameter.

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

static_assert (!is_function_parameter (null_reflection));
static_assert (!is_function_parameter (^^::));
static_assert (!is_function_parameter (^^ns));
static_assert (!is_function_parameter (^^ns_alias));
static_assert (!is_function_parameter (reflect_constant (3)));
static_assert (!is_function_parameter (^^cls));
static_assert (!is_function_parameter (^^cls::dm));
static_assert (!is_function_parameter (^^cls::ref_dm));
static_assert (!is_function_parameter (^^cls::static_dm));
static_assert (!is_function_parameter (^^cls::mem_fun));
static_assert (!is_function_parameter (^^cls::static_mem_fun));
static_assert (!is_function_parameter (^^cls::type));
static_assert (!is_function_parameter (^^cls_var));
static_assert (!is_function_parameter (^^onion));
static_assert (!is_function_parameter (^^anon));
static_assert (!is_function_parameter (^^fun));
static_assert (!is_function_parameter (^^alias));
static_assert (!is_function_parameter (^^var));
static_assert (!is_function_parameter (^^ref));
static_assert (!is_function_parameter (^^rref));
static_assert (!is_function_parameter (^^ptr));
static_assert (!is_function_parameter (^^cls_tmpl));
static_assert (!is_function_parameter (^^cls_tmpl<int>));
static_assert (!is_function_parameter (^^incomplete_cls<int>));
static_assert (!is_function_parameter (^^fun_tmpl));
static_assert (!is_function_parameter (^^fun_tmpl<int>));
static_assert (!is_function_parameter (^^conc));
static_assert (!is_function_parameter (substitute (^^conc, { ^^int })));
static_assert (!is_function_parameter (^^var_tmpl));
static_assert (!is_function_parameter (^^var_tmpl<int>));
static_assert (!is_function_parameter (^^cls_tmpl_alias));
static_assert (!is_function_parameter (^^cls_tmpl_alias<int>));
static_assert (!is_function_parameter (^^Enum));
static_assert (!is_function_parameter (^^Enum::A));
static_assert (!is_function_parameter (^^Enum_class));
static_assert (!is_function_parameter (^^Enum_class::A));
static_assert (!is_function_parameter (^^decomp));
static_assert (!is_function_parameter (^^decomp_ref));
static_assert (!is_function_parameter (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_function_parameter (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_function_parameter (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_function_parameter (^^T));
  static_assert (!is_function_parameter (R));
  static_assert (!is_function_parameter (R2));
  static_assert (!is_function_parameter (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^p, ^^c>();
  static_assert (!is_function_parameter (^^p));
  static_assert (!is_function_parameter (^^c));
}
