// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_namespace_alias.

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

static_assert (!is_namespace_alias (null_reflection));
static_assert (!is_namespace_alias (^^::));
static_assert (!is_namespace_alias (^^ns));
static_assert (is_namespace_alias (^^ns_alias));
static_assert (!is_namespace_alias (reflect_constant (3)));
static_assert (!is_namespace_alias (^^cls));
static_assert (!is_namespace_alias (^^cls::dm));
static_assert (!is_namespace_alias (^^cls::ref_dm));
static_assert (!is_namespace_alias (^^cls::static_dm));
static_assert (!is_namespace_alias (^^cls::mem_fun));
static_assert (!is_namespace_alias (^^cls::static_mem_fun));
static_assert (!is_namespace_alias (^^cls::type));
static_assert (!is_namespace_alias (^^cls_var));
static_assert (!is_namespace_alias (^^onion));
static_assert (!is_namespace_alias (^^anon));
static_assert (!is_namespace_alias (^^fun));
static_assert (!is_namespace_alias (^^alias));
static_assert (!is_namespace_alias (^^var));
static_assert (!is_namespace_alias (^^ref));
static_assert (!is_namespace_alias (^^rref));
static_assert (!is_namespace_alias (^^ptr));
static_assert (!is_namespace_alias (^^cls_tmpl));
static_assert (!is_namespace_alias (^^cls_tmpl<int>));
static_assert (!is_namespace_alias (^^incomplete_cls<int>));
static_assert (!is_namespace_alias (^^fun_tmpl));
static_assert (!is_namespace_alias (^^fun_tmpl<int>));
static_assert (!is_namespace_alias (^^conc));
static_assert (!is_namespace_alias (substitute (^^conc, { ^^int })));
static_assert (!is_namespace_alias (^^var_tmpl));
static_assert (!is_namespace_alias (^^var_tmpl<int>));
static_assert (!is_namespace_alias (^^cls_tmpl_alias));
static_assert (!is_namespace_alias (^^cls_tmpl_alias<int>));
static_assert (!is_namespace_alias (^^Enum));
static_assert (!is_namespace_alias (^^Enum::A));
static_assert (!is_namespace_alias (^^Enum_class));
static_assert (!is_namespace_alias (^^Enum_class::A));
static_assert (!is_namespace_alias (^^decomp));
static_assert (!is_namespace_alias (^^decomp_ref));
static_assert (!is_namespace_alias (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_namespace_alias (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_namespace_alias (bases_of (^^Derived, access_context::unchecked ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_namespace_alias (^^T));
  static_assert (!is_namespace_alias (R));
  static_assert (!is_namespace_alias (R2));
  static_assert (is_namespace_alias (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^ns_alias>();
  static_assert (!is_namespace_alias (^^p));
  static_assert (!is_namespace_alias (^^c));
}
