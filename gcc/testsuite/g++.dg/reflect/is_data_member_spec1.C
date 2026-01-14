// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_data_member_spec.

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

static_assert (!is_data_member_spec (null_reflection));
static_assert (!is_data_member_spec (^^::));
static_assert (!is_data_member_spec (^^ns));
static_assert (!is_data_member_spec (^^ns_alias));
static_assert (!is_data_member_spec (reflect_constant (3)));
static_assert (!is_data_member_spec (^^cls));
static_assert (!is_data_member_spec (^^cls::dm));
static_assert (!is_data_member_spec (^^cls::ref_dm));
static_assert (!is_data_member_spec (^^cls::static_dm));
static_assert (!is_data_member_spec (^^cls::mem_fun));
static_assert (!is_data_member_spec (^^cls::static_mem_fun));
static_assert (!is_data_member_spec (^^cls::type));
static_assert (!is_data_member_spec (^^cls_var));
static_assert (!is_data_member_spec (^^onion));
static_assert (!is_data_member_spec (^^anon));
static_assert (!is_data_member_spec (^^fun));
static_assert (!is_data_member_spec (^^alias));
static_assert (!is_data_member_spec (^^var));
static_assert (!is_data_member_spec (^^ref));
static_assert (!is_data_member_spec (^^rref));
static_assert (!is_data_member_spec (^^ptr));
static_assert (!is_data_member_spec (^^cls_tmpl));
static_assert (!is_data_member_spec (^^cls_tmpl<int>));
static_assert (!is_data_member_spec (^^incomplete_cls<int>));
static_assert (!is_data_member_spec (^^fun_tmpl));
static_assert (!is_data_member_spec (^^fun_tmpl<int>));
static_assert (!is_data_member_spec (^^conc));
static_assert (!is_data_member_spec (substitute (^^conc, { ^^int })));
static_assert (!is_data_member_spec (^^var_tmpl));
static_assert (!is_data_member_spec (^^var_tmpl<int>));
static_assert (!is_data_member_spec (^^cls_tmpl_alias));
static_assert (!is_data_member_spec (^^cls_tmpl_alias<int>));
static_assert (!is_data_member_spec (^^Enum));
static_assert (!is_data_member_spec (^^Enum::A));
static_assert (!is_data_member_spec (^^Enum_class));
static_assert (!is_data_member_spec (^^Enum_class::A));
static_assert (!is_data_member_spec (^^decomp));
static_assert (!is_data_member_spec (^^decomp_ref));
static_assert (!is_data_member_spec (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (is_data_member_spec (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_data_member_spec (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_data_member_spec (^^T));
  static_assert (!is_data_member_spec (R));
  static_assert (!is_data_member_spec (R2));
  static_assert (!is_data_member_spec (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!is_data_member_spec (^^p));
  static_assert (!is_data_member_spec (^^c));
}
