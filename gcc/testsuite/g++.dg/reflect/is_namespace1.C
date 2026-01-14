// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_namespace.

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

static_assert (!is_namespace (null_reflection));
static_assert (is_namespace (^^::));
static_assert (is_namespace (^^ns));
static_assert (is_namespace (^^ns_alias));
static_assert (!is_namespace (reflect_constant (3)));
static_assert (!is_namespace (^^cls));
static_assert (!is_namespace (^^cls::dm));
static_assert (!is_namespace (^^cls::ref_dm));
static_assert (!is_namespace (^^cls::static_dm));
static_assert (!is_namespace (^^cls::mem_fun));
static_assert (!is_namespace (^^cls::static_mem_fun));
static_assert (!is_namespace (^^cls::type));
static_assert (!is_namespace (^^cls_var));
static_assert (!is_namespace (^^onion));
static_assert (!is_namespace (^^anon));
static_assert (!is_namespace (^^fun));
static_assert (!is_namespace (^^alias));
static_assert (!is_namespace (^^var));
static_assert (!is_namespace (^^ref));
static_assert (!is_namespace (^^rref));
static_assert (!is_namespace (^^ptr));
static_assert (!is_namespace (^^cls_tmpl));
static_assert (!is_namespace (^^cls_tmpl<int>));
static_assert (!is_namespace (^^incomplete_cls<int>));
static_assert (!is_namespace (^^fun_tmpl));
static_assert (!is_namespace (^^fun_tmpl<int>));
static_assert (!is_namespace (^^conc));
static_assert (!is_namespace (substitute (^^conc, { ^^int })));
static_assert (!is_namespace (^^var_tmpl));
static_assert (!is_namespace (^^var_tmpl<int>));
static_assert (!is_namespace (^^cls_tmpl_alias));
static_assert (!is_namespace (^^cls_tmpl_alias<int>));
static_assert (!is_namespace (^^Enum));
static_assert (!is_namespace (^^Enum::A));
static_assert (!is_namespace (^^Enum_class));
static_assert (!is_namespace (^^Enum_class::A));
static_assert (!is_namespace (^^decomp));
static_assert (!is_namespace (^^decomp_ref));
static_assert (!is_namespace (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_namespace (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_namespace (bases_of (^^Derived, access_context::unprivileged ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_namespace (^^T));
  static_assert (!is_namespace (R));
  static_assert (is_namespace (R2));
  static_assert (!is_namespace (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!is_namespace (^^p));
  static_assert (!is_namespace (^^c));
}
