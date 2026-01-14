// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_{static,thread,automatic}_storage_duration.

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

static_assert (!has_static_storage_duration (null_reflection));
static_assert (!has_thread_storage_duration (null_reflection));
static_assert (!has_automatic_storage_duration (null_reflection));

static_assert (!has_static_storage_duration (^^::));
static_assert (!has_thread_storage_duration (^^::));
static_assert (!has_automatic_storage_duration (^^::));

static_assert (!has_static_storage_duration (reflect_constant (3)));
static_assert (!has_thread_storage_duration (reflect_constant (3)));
static_assert (!has_automatic_storage_duration (reflect_constant (3)));

static_assert (!has_static_storage_duration (^^cls));
static_assert (!has_thread_storage_duration (^^cls));
static_assert (!has_automatic_storage_duration (^^cls));

static_assert (!has_static_storage_duration (^^cls::dm));
static_assert (!has_thread_storage_duration (^^cls::dm));
static_assert (!has_automatic_storage_duration (^^cls::dm));

static_assert (!has_static_storage_duration (^^cls::ref_dm));
static_assert (!has_thread_storage_duration (^^cls::ref_dm));
static_assert (!has_automatic_storage_duration (^^cls::ref_dm));

static_assert (has_static_storage_duration (^^cls::static_dm));
static_assert (!has_thread_storage_duration (^^cls::static_dm));
static_assert (!has_automatic_storage_duration (^^cls::static_dm));


static_assert (!has_static_storage_duration (^^cls::mem_fun));
static_assert (!has_thread_storage_duration (^^cls::mem_fun));
static_assert (!has_automatic_storage_duration (^^cls::mem_fun));

static_assert (!has_static_storage_duration (^^cls::static_mem_fun));
static_assert (!has_thread_storage_duration (^^cls::static_mem_fun));
static_assert (!has_automatic_storage_duration (^^cls::static_mem_fun));

static_assert (!has_static_storage_duration (^^cls::type));
static_assert (!has_thread_storage_duration (^^cls::type));
static_assert (!has_automatic_storage_duration (^^cls::type));

static_assert (has_static_storage_duration (^^cls_var));
static_assert (!has_thread_storage_duration (^^cls_var));
static_assert (!has_automatic_storage_duration (^^cls_var));

static_assert (!has_static_storage_duration (^^onion));
static_assert (!has_thread_storage_duration (^^onion));
static_assert (!has_automatic_storage_duration (^^onion));

static_assert (!has_static_storage_duration (^^anon));
static_assert (!has_thread_storage_duration (^^anon));
static_assert (!has_automatic_storage_duration (^^anon));

static_assert (!has_static_storage_duration (^^fun));
static_assert (!has_thread_storage_duration (^^fun));
static_assert (!has_automatic_storage_duration (^^fun));

static_assert (!has_static_storage_duration (^^alias));
static_assert (!has_thread_storage_duration (^^alias));
static_assert (!has_automatic_storage_duration (^^alias));

static_assert (has_static_storage_duration (^^var));
static_assert (!has_thread_storage_duration (^^var));
static_assert (!has_automatic_storage_duration (^^var));

static_assert (has_static_storage_duration (^^ref));
static_assert (!has_thread_storage_duration (^^ref));
static_assert (!has_automatic_storage_duration (^^ref));

static_assert (has_static_storage_duration (^^rref));
static_assert (!has_thread_storage_duration (^^rref));
static_assert (!has_automatic_storage_duration (^^rref));

static_assert (has_static_storage_duration (^^ptr));
static_assert (!has_thread_storage_duration (^^ptr));
static_assert (!has_automatic_storage_duration (^^ptr));

static_assert (!has_static_storage_duration (^^cls_tmpl));
static_assert (!has_thread_storage_duration (^^cls_tmpl));
static_assert (!has_automatic_storage_duration (^^cls_tmpl));

static_assert (!has_static_storage_duration (^^cls_tmpl<int>));
static_assert (!has_thread_storage_duration (^^cls_tmpl<int>));
static_assert (!has_automatic_storage_duration (^^cls_tmpl<int>));

static_assert (!has_static_storage_duration (^^incomplete_cls<int>));
static_assert (!has_thread_storage_duration (^^incomplete_cls<int>));
static_assert (!has_automatic_storage_duration (^^incomplete_cls<int>));

static_assert (!has_static_storage_duration (^^fun_tmpl));
static_assert (!has_thread_storage_duration (^^fun_tmpl));
static_assert (!has_automatic_storage_duration (^^fun_tmpl));

static_assert (!has_static_storage_duration (^^fun_tmpl<int>));
static_assert (!has_thread_storage_duration (^^fun_tmpl<int>));
static_assert (!has_automatic_storage_duration (^^fun_tmpl<int>));

static_assert (!has_static_storage_duration (^^conc));
static_assert (!has_thread_storage_duration (^^conc));
static_assert (!has_automatic_storage_duration (^^conc));

static_assert (!has_static_storage_duration (substitute (^^conc, {^^int})));
static_assert (!has_thread_storage_duration (substitute (^^conc, {^^int})));
static_assert (!has_automatic_storage_duration (substitute (^^conc, {^^int})));

static_assert (!has_static_storage_duration (^^var_tmpl));
static_assert (!has_thread_storage_duration (^^var_tmpl));
static_assert (!has_automatic_storage_duration (^^var_tmpl));

static_assert (has_static_storage_duration (^^var_tmpl<int>));
static_assert (!has_thread_storage_duration (^^var_tmpl<int>));
static_assert (!has_automatic_storage_duration (^^var_tmpl<int>));

static_assert (!has_static_storage_duration (^^cls_tmpl_alias));
static_assert (!has_thread_storage_duration (^^cls_tmpl_alias));
static_assert (!has_automatic_storage_duration (^^cls_tmpl_alias));

static_assert (!has_static_storage_duration (^^cls_tmpl_alias<int>));
static_assert (!has_thread_storage_duration (^^cls_tmpl_alias<int>));
static_assert (!has_automatic_storage_duration (^^cls_tmpl_alias<int>));

static_assert (!has_static_storage_duration (^^Enum));
static_assert (!has_thread_storage_duration (^^Enum));
static_assert (!has_automatic_storage_duration (^^Enum));

static_assert (!has_static_storage_duration (^^Enum::A));
static_assert (!has_thread_storage_duration (^^Enum::A));
static_assert (!has_automatic_storage_duration (^^Enum::A));

static_assert (!has_static_storage_duration (^^Enum_class));
static_assert (!has_thread_storage_duration (^^Enum_class));
static_assert (!has_automatic_storage_duration (^^Enum_class));

static_assert (!has_static_storage_duration (^^Enum_class::A));
static_assert (!has_thread_storage_duration (^^Enum_class::A));
static_assert (!has_automatic_storage_duration (^^Enum_class::A));

static_assert (!has_static_storage_duration (^^decomp));
static_assert (!has_thread_storage_duration (^^decomp));
static_assert (!has_automatic_storage_duration (^^decomp));

static_assert (!has_static_storage_duration (^^decomp_ref));
static_assert (!has_thread_storage_duration (^^decomp_ref));
static_assert (!has_automatic_storage_duration (^^decomp_ref));

static_assert (has_static_storage_duration (^^arr));
static_assert (!has_thread_storage_duration (^^arr));
static_assert (!has_automatic_storage_duration (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!has_static_storage_duration (dms));
static_assert (!has_thread_storage_duration (dms));
static_assert (!has_automatic_storage_duration (dms));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_static_storage_duration (^^T));
  static_assert (!has_thread_storage_duration (^^T));
  static_assert (!has_automatic_storage_duration (^^T));
  static_assert (has_static_storage_duration (R));
  static_assert (!has_thread_storage_duration (R));
  static_assert (!has_automatic_storage_duration (R));
  static_assert (!has_static_storage_duration (R2));
  static_assert (!has_thread_storage_duration (R2));
  static_assert (!has_automatic_storage_duration (R2));
  static_assert (!has_static_storage_duration (R3));
  static_assert (!has_thread_storage_duration (R3));
  static_assert (!has_automatic_storage_duration (R3));
}

void
g ()
{
  f<int, ^^var, ^^ns, ^^cls>();
}
