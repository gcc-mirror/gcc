// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_object.

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

static_assert (!is_object (null_reflection));
static_assert (!is_object (^^::));
static_assert (!is_object (^^ns));
static_assert (!is_object (^^ns_alias));
static_assert (!is_object (reflect_constant (3)));
static_assert (!is_object (^^cls));
static_assert (!is_object (^^cls::dm));
static_assert (!is_object (^^cls::ref_dm));
static_assert (!is_object (^^cls::static_dm));
static_assert (!is_object (^^cls::mem_fun));
static_assert (!is_object (^^cls::static_mem_fun));
static_assert (!is_object (^^cls::type));
static_assert (!is_object (^^cls_var));
static_assert (!is_object (^^onion));
static_assert (!is_object (^^anon));
static_assert (!is_object (^^fun));
static_assert (!is_object (^^alias));
static_assert (!is_object (^^var));
static_assert (!is_object (^^ref));
static_assert (!is_object (^^rref));
static_assert (!is_object (^^ptr));
static_assert (!is_object (^^cls_tmpl));
static_assert (!is_object (^^cls_tmpl<int>));
static_assert (!is_object (^^incomplete_cls<int>));
static_assert (!is_object (^^fun_tmpl));
static_assert (!is_object (^^fun_tmpl<int>));
static_assert (!is_object (^^conc));
static_assert (!is_object (substitute (^^conc, { ^^int })));
static_assert (!is_object (^^var_tmpl));
static_assert (!is_object (^^var_tmpl<int>));
static_assert (!is_object (^^cls_tmpl_alias));
static_assert (!is_object (^^cls_tmpl_alias<int>));
static_assert (!is_object (^^Enum));
static_assert (!is_object (^^Enum::A));
static_assert (!is_object (^^Enum_class));
static_assert (!is_object (^^Enum_class::A));
static_assert (!is_object (^^decomp));
static_assert (!is_object (^^decomp_ref));
static_assert (!is_object (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_object (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_object (bases_of (^^Derived, access_context::unchecked ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_object (^^T));
  static_assert (!is_object (R));
  static_assert (!is_object (R2));
  static_assert (!is_object (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!is_object (^^p));
  static_assert (!is_object (^^c));
}
