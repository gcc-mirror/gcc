// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_complete_type.

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
using inc_arr = int[];
using com_arr = int[42];
auto [ decomp ] = arr;
auto &[ decomp_ref ] = arr;

static_assert (!is_complete_type (null_reflection));
static_assert (!is_complete_type (^^::));
static_assert (!is_complete_type (^^ns));
static_assert (!is_complete_type (^^ns_alias));
static_assert (!is_complete_type (reflect_constant (3)));
static_assert (is_complete_type (^^cls));
static_assert (!is_complete_type (^^cls::dm));
static_assert (!is_complete_type (^^cls::ref_dm));
static_assert (!is_complete_type (^^cls::static_dm));
static_assert (!is_complete_type (^^cls::mem_fun));
static_assert (!is_complete_type (^^cls::static_mem_fun));
static_assert (is_complete_type (^^cls::type));
static_assert (!is_complete_type (^^cls_var));
static_assert (is_complete_type (^^onion));
static_assert (!is_complete_type (^^anon));
static_assert (!is_complete_type (^^fun));
static_assert (is_complete_type (^^alias));
static_assert (!is_complete_type (^^var));
static_assert (!is_complete_type (^^ref));
static_assert (!is_complete_type (^^rref));
static_assert (!is_complete_type (^^ptr));
static_assert (!is_complete_type (^^cls_tmpl));
static_assert (is_complete_type (^^cls_tmpl<int>));
static_assert (!is_complete_type (^^incomplete_cls<int>));
static_assert (!is_complete_type (^^fun_tmpl));
static_assert (!is_complete_type (^^fun_tmpl<int>));
static_assert (!is_complete_type (^^conc));
static_assert (!is_complete_type (substitute (^^conc, { ^^int })));
static_assert (!is_complete_type (^^var_tmpl));
static_assert (!is_complete_type (^^var_tmpl<int>));
static_assert (!is_complete_type (^^cls_tmpl_alias));
static_assert (is_complete_type (^^cls_tmpl_alias<int>));
static_assert (is_complete_type (^^Enum));
static_assert (!is_complete_type (^^Enum::A));
static_assert (is_complete_type (^^Enum_class));
static_assert (!is_complete_type (^^Enum_class::A));
static_assert (!is_complete_type (^^decomp));
static_assert (!is_complete_type (^^decomp_ref));
static_assert (!is_complete_type (^^arr));
static_assert (!is_complete_type (^^inc_arr));
static_assert (is_complete_type (^^com_arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_complete_type (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_complete_type (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (is_complete_type (^^T));
  static_assert (!is_complete_type (R));
  static_assert (!is_complete_type (R2));
  static_assert (is_complete_type (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!is_complete_type (^^p));
  static_assert (!is_complete_type (^^c));
}
