// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_variable_template.

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

static_assert (!is_variable_template (null_reflection));
static_assert (!is_variable_template (^^::));
static_assert (!is_variable_template (^^ns));
static_assert (!is_variable_template (^^ns_alias));
static_assert (!is_variable_template (reflect_constant (3)));
static_assert (!is_variable_template (^^cls));
static_assert (!is_variable_template (^^cls::dm));
static_assert (!is_variable_template (^^cls::ref_dm));
static_assert (!is_variable_template (^^cls::static_dm));
static_assert (!is_variable_template (^^cls::mem_fun));
static_assert (!is_variable_template (^^cls::static_mem_fun));
static_assert (!is_variable_template (^^cls::type));
static_assert (!is_variable_template (^^cls_var));
static_assert (!is_variable_template (^^onion));
static_assert (!is_variable_template (^^anon));
static_assert (!is_variable_template (^^fun));
static_assert (!is_variable_template (^^alias));
static_assert (!is_variable_template (^^var));
static_assert (!is_variable_template (^^ref));
static_assert (!is_variable_template (^^rref));
static_assert (!is_variable_template (^^ptr));
static_assert (!is_variable_template (^^cls_tmpl));
static_assert (!is_variable_template (^^cls_tmpl<int>));
static_assert (!is_variable_template (^^incomplete_cls<int>));
static_assert (!is_variable_template (^^fun_tmpl));
static_assert (!is_variable_template (^^fun_tmpl<int>));
static_assert (!is_variable_template (^^conc));
static_assert (!is_variable_template (substitute (^^conc, { ^^int })));
static_assert (is_variable_template (^^var_tmpl));
static_assert (!is_variable_template (^^var_tmpl<int>));
static_assert (!is_variable_template (^^cls_tmpl_alias));
static_assert (!is_variable_template (^^cls_tmpl_alias<int>));
static_assert (!is_variable_template (^^Enum));
static_assert (!is_variable_template (^^Enum::A));
static_assert (!is_variable_template (^^Enum_class));
static_assert (!is_variable_template (^^Enum_class::A));
static_assert (!is_variable_template (^^decomp));
static_assert (!is_variable_template (^^decomp_ref));
static_assert (!is_variable_template (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_variable_template (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_variable_template (bases_of (^^Derived, access_context::unchecked ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_variable_template (^^T));
  static_assert (!is_variable_template (R));
  static_assert (!is_variable_template (R2));
  static_assert (is_variable_template (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^var_tmpl>();
  static_assert (!is_variable_template (^^p));
  static_assert (!is_variable_template (^^c));
}
