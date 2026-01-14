// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function.

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

static_assert (!is_function (null_reflection));
static_assert (!is_function (^^::));
static_assert (!is_function (^^ns));
static_assert (!is_function (^^ns_alias));
static_assert (!is_function (reflect_constant (3)));
static_assert (!is_function (^^cls));
static_assert (!is_function (^^cls::dm));
static_assert (!is_function (^^cls::ref_dm));
static_assert (!is_function (^^cls::static_dm));
static_assert (is_function (^^cls::mem_fun));
static_assert (is_function (^^cls::static_mem_fun));
static_assert (!is_function (^^cls::type));
static_assert (!is_function (^^cls_var));
static_assert (!is_function (^^onion));
static_assert (!is_function (^^anon));
static_assert (is_function (^^fun));
static_assert (!is_function (^^alias));
static_assert (!is_function (^^var));
static_assert (!is_function (^^ref));
static_assert (!is_function (^^rref));
static_assert (!is_function (^^ptr));
static_assert (!is_function (^^cls_tmpl));
static_assert (!is_function (^^cls_tmpl<int>));
static_assert (!is_function (^^incomplete_cls<int>));
static_assert (!is_function (^^fun_tmpl));
static_assert (is_function (^^fun_tmpl<int>));
static_assert (!is_function (^^conc));
static_assert (!is_function (substitute (^^conc, { ^^int })));
static_assert (!is_function (^^var_tmpl));
static_assert (!is_function (^^var_tmpl<int>));
static_assert (!is_function (^^cls_tmpl_alias));
static_assert (!is_function (^^cls_tmpl_alias<int>));
static_assert (!is_function (^^Enum));
static_assert (!is_function (^^Enum::A));
static_assert (!is_function (^^Enum_class));
static_assert (!is_function (^^Enum_class::A));
static_assert (!is_function (^^decomp));
static_assert (!is_function (^^decomp_ref));
static_assert (!is_function (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_function (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_function (bases_of (^^Derived, access_context::unprivileged ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_function (^^T));
  static_assert (!is_function (R));
  static_assert (!is_function (R2));
  static_assert (is_function (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^fun>();
  static_assert (!is_function (^^p));
  static_assert (!is_function (^^c));
}
