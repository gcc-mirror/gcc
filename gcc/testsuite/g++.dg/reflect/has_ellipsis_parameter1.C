// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_ellipsis_parameter.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
struct cls {
  int dm;
  static int static_dm;
  void mem_fun ();
  void mem_fun2 (int, ...);
  void mem_fun3 (...);
  void mem_fun4 (int);
  static void static_mem_fun ();
  static void static_mem_fun2 (int, long, ...);
  static void static_mem_fun3 (...);
  static void static_mem_fun4 (long);
  int &ref_dm = dm;
  using type = int;
} cls_var;
union onion { };
static union { int anon; };
using alias = cls;
void fun ();
void fun2 (int, ...);
void fun3 (...);
void fun4 (int, int);
int var;
int &ref = var;
int &&rref = 42;
int *ptr = &var;
namespace ns {}
namespace ns_alias = ns;
enum Enum { A };
enum class Enum_class { A };
using funt = int ();
using funt2 = int (int, ...);
using funt3 = int (...);
using funt4 = int (int, long);

template<typename> struct incomplete_cls;
template<typename> struct cls_tmpl {};
template<typename> void fun_tmpl ();
template<typename> void fun_tmpl2 (int, ...);
template<typename> void fun_tmpl3 (...);
template<typename> void fun_tmpl4 (long, int);
template<typename> concept conc = requires { true; };
template<typename> int var_tmpl;
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;

int arr[] = { 42 };
using inc_arr = int[];
using com_arr = int[42];
auto [ decomp ] = arr;
auto &[ decomp_ref ] = arr;

static_assert (!has_ellipsis_parameter (null_reflection));
static_assert (!has_ellipsis_parameter (^^::));
static_assert (!has_ellipsis_parameter (^^ns));
static_assert (!has_ellipsis_parameter (^^ns_alias));
static_assert (!has_ellipsis_parameter (reflect_constant (3)));
static_assert (!has_ellipsis_parameter (^^cls));
static_assert (!has_ellipsis_parameter (^^cls::dm));
static_assert (!has_ellipsis_parameter (^^cls::ref_dm));
static_assert (!has_ellipsis_parameter (^^cls::static_dm));
static_assert (!has_ellipsis_parameter (^^cls::mem_fun));
static_assert (has_ellipsis_parameter (^^cls::mem_fun2));
static_assert (has_ellipsis_parameter (^^cls::mem_fun3));
static_assert (!has_ellipsis_parameter (^^cls::mem_fun4));
static_assert (!has_ellipsis_parameter (^^cls::static_mem_fun));
static_assert (has_ellipsis_parameter (^^cls::static_mem_fun2));
static_assert (has_ellipsis_parameter (^^cls::static_mem_fun3));
static_assert (!has_ellipsis_parameter (^^cls::static_mem_fun4));
static_assert (!has_ellipsis_parameter (^^cls::type));
static_assert (!has_ellipsis_parameter (^^cls_var));
static_assert (!has_ellipsis_parameter (^^onion));
static_assert (!has_ellipsis_parameter (^^anon));
static_assert (!has_ellipsis_parameter (^^fun));
static_assert (has_ellipsis_parameter (^^fun2));
static_assert (has_ellipsis_parameter (^^fun3));
static_assert (!has_ellipsis_parameter (^^fun4));
static_assert (!has_ellipsis_parameter (type_of (^^fun)));
static_assert (has_ellipsis_parameter (type_of (^^fun2)));
static_assert (has_ellipsis_parameter (type_of (^^fun3)));
static_assert (!has_ellipsis_parameter (type_of (^^fun4)));
static_assert (!has_ellipsis_parameter (^^alias));
static_assert (!has_ellipsis_parameter (^^var));
static_assert (!has_ellipsis_parameter (^^ref));
static_assert (!has_ellipsis_parameter (^^rref));
static_assert (!has_ellipsis_parameter (^^ptr));
static_assert (!has_ellipsis_parameter (^^cls_tmpl));
static_assert (!has_ellipsis_parameter (^^cls_tmpl<int>));
static_assert (!has_ellipsis_parameter (^^incomplete_cls<int>));
static_assert (!has_ellipsis_parameter (^^fun_tmpl));
static_assert (!has_ellipsis_parameter (^^fun_tmpl<int>));
static_assert (!has_ellipsis_parameter (^^fun_tmpl2));
static_assert (has_ellipsis_parameter (^^fun_tmpl2<int>));
static_assert (!has_ellipsis_parameter (^^fun_tmpl3));
static_assert (has_ellipsis_parameter (^^fun_tmpl3<int>));
static_assert (!has_ellipsis_parameter (^^fun_tmpl4));
static_assert (!has_ellipsis_parameter (^^fun_tmpl4<int>));
static_assert (!has_ellipsis_parameter (^^conc));
static_assert (!has_ellipsis_parameter (substitute (^^conc, { ^^int })));
static_assert (!has_ellipsis_parameter (^^var_tmpl));
static_assert (!has_ellipsis_parameter (^^var_tmpl<int>));
static_assert (!has_ellipsis_parameter (^^cls_tmpl_alias));
static_assert (!has_ellipsis_parameter (^^cls_tmpl_alias<int>));
static_assert (!has_ellipsis_parameter (^^Enum));
static_assert (!has_ellipsis_parameter (^^Enum::A));
static_assert (!has_ellipsis_parameter (^^Enum_class));
static_assert (!has_ellipsis_parameter (^^Enum_class::A));
static_assert (!has_ellipsis_parameter (^^decomp));
static_assert (!has_ellipsis_parameter (^^decomp_ref));
static_assert (!has_ellipsis_parameter (^^arr));
static_assert (!has_ellipsis_parameter (^^inc_arr));
static_assert (!has_ellipsis_parameter (^^com_arr));
static_assert (!has_ellipsis_parameter (^^funt));
static_assert (has_ellipsis_parameter (^^funt2));
static_assert (has_ellipsis_parameter (^^funt3));
static_assert (!has_ellipsis_parameter (^^funt4));

constexpr auto dms = data_member_spec (^^int, { .name = u8"a" });
static_assert (!has_ellipsis_parameter (dms));

struct Base {};
struct Derived : Base {};
static_assert (!has_ellipsis_parameter (bases_of (^^Derived, access_context::unchecked ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_ellipsis_parameter (^^T));
  static_assert (!has_ellipsis_parameter (R));
  static_assert (!has_ellipsis_parameter (R2));
  static_assert (!has_ellipsis_parameter (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!has_ellipsis_parameter (^^p));
  static_assert (!has_ellipsis_parameter (^^c));
}
