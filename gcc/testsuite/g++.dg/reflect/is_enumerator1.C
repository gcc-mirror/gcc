// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_enumerator.

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

static_assert (!is_enumerator (null_reflection));
static_assert (!is_enumerator (^^::));
static_assert (!is_enumerator (^^ns));
static_assert (!is_enumerator (^^ns_alias));
static_assert (!is_enumerator (reflect_constant (3)));
static_assert (!is_enumerator (^^cls));
static_assert (!is_enumerator (^^cls::dm));
static_assert (!is_enumerator (^^cls::ref_dm));
static_assert (!is_enumerator (^^cls::static_dm));
static_assert (!is_enumerator (^^cls::mem_fun));
static_assert (!is_enumerator (^^cls::static_mem_fun));
static_assert (!is_enumerator (^^cls::type));
static_assert (!is_enumerator (^^cls_var));
static_assert (!is_enumerator (^^onion));
static_assert (!is_enumerator (^^anon));
static_assert (!is_enumerator (^^fun));
static_assert (!is_enumerator (^^alias));
static_assert (!is_enumerator (^^var));
static_assert (!is_enumerator (^^ref));
static_assert (!is_enumerator (^^rref));
static_assert (!is_enumerator (^^ptr));
static_assert (!is_enumerator (^^cls_tmpl));
static_assert (!is_enumerator (^^cls_tmpl<int>));
static_assert (!is_enumerator (^^incomplete_cls<int>));
static_assert (!is_enumerator (^^fun_tmpl));
static_assert (!is_enumerator (^^fun_tmpl<int>));
static_assert (!is_enumerator (^^conc));
static_assert (!is_enumerator (substitute (^^conc, { ^^int })));
static_assert (!is_enumerator (^^var_tmpl));
static_assert (!is_enumerator (^^var_tmpl<int>));
static_assert (!is_enumerator (^^cls_tmpl_alias));
static_assert (!is_enumerator (^^cls_tmpl_alias<int>));
static_assert (!is_enumerator (^^Enum));
static_assert (is_enumerator (^^Enum::A));
static_assert (!is_enumerator (^^Enum_class));
static_assert (is_enumerator (^^Enum_class::A));
static_assert (!is_enumerator (^^decomp));
static_assert (!is_enumerator (^^decomp_ref));
static_assert (!is_enumerator (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_enumerator (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_enumerator (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_enumerator (^^T));
  static_assert (!is_enumerator (R));
  static_assert (is_enumerator (R2));
  static_assert (is_enumerator (R3));
}

void
g (int p, cls c, Enum e, Enum_class ec)
{
  f<int, ^^var, ^^Enum::A, ^^Enum_class::A>();
  static_assert (!is_enumerator (^^p));
  static_assert (!is_enumerator (^^c));
  static_assert (!is_enumerator (^^e));
  static_assert (!is_enumerator (^^ec));
}
