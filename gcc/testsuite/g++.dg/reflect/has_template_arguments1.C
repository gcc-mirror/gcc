// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_template_arguments.

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

using U = cls_tmpl<int>;
typedef cls_tmpl<int> TYPE;

int arr[] = { 42 };
auto [ decomp ] = arr;
auto &[ decomp_ref ] = arr;

static_assert (!has_template_arguments (^^int));
static_assert (!has_template_arguments (null_reflection));
static_assert (!has_template_arguments (^^::));
static_assert (!has_template_arguments (^^ns));
static_assert (!has_template_arguments (^^ns_alias));
static_assert (!has_template_arguments (reflect_constant (3)));
static_assert (!has_template_arguments (^^cls));
static_assert (!has_template_arguments (^^cls::dm));
static_assert (!has_template_arguments (^^cls::ref_dm));
static_assert (!has_template_arguments (^^cls::static_dm));
static_assert (!has_template_arguments (^^cls::mem_fun));
static_assert (!has_template_arguments (^^cls::static_mem_fun));
static_assert (!has_template_arguments (^^cls::type));
static_assert (!has_template_arguments (^^cls_var));
static_assert (!has_template_arguments (^^onion));
static_assert (!has_template_arguments (^^anon));
static_assert (!has_template_arguments (^^fun));
static_assert (!has_template_arguments (^^alias));
static_assert (!has_template_arguments (^^var));
static_assert (!has_template_arguments (^^ref));
static_assert (!has_template_arguments (^^rref));
static_assert (!has_template_arguments (^^ptr));
static_assert (!has_template_arguments (^^cls_tmpl));
static_assert (has_template_arguments (^^cls_tmpl<int>));
static_assert (has_template_arguments (^^incomplete_cls<int>));
static_assert (!has_template_arguments (^^fun_tmpl));
static_assert (has_template_arguments (^^fun_tmpl<int>));
static_assert (!has_template_arguments (^^conc));
static_assert (!has_template_arguments (substitute (^^conc, { ^^int })));
static_assert (!has_template_arguments (^^var_tmpl));
static_assert (has_template_arguments (^^var_tmpl<int>));
static_assert (!has_template_arguments (^^cls_tmpl_alias));
static_assert (has_template_arguments (^^cls_tmpl_alias<int>));
static_assert (!has_template_arguments (^^Enum));
static_assert (!has_template_arguments (^^Enum::A));
static_assert (!has_template_arguments (^^Enum_class));
static_assert (!has_template_arguments (^^Enum_class::A));
static_assert (!has_template_arguments (^^decomp));
static_assert (!has_template_arguments (^^decomp_ref));
static_assert (!has_template_arguments (^^arr));
static_assert (!has_template_arguments (^^U));
static_assert (!has_template_arguments (^^TYPE));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!has_template_arguments (dms));

struct Base {};
struct Derived : Base {};
static_assert (!has_template_arguments (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_template_arguments (^^T));
  static_assert (!has_template_arguments (R));
  static_assert (has_template_arguments (R2));
  static_assert (has_template_arguments (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^var_tmpl<int>, ^^cls_tmpl<int>>();
  static_assert (!has_template_arguments (^^p));
  static_assert (!has_template_arguments (^^c));
}
