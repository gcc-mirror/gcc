// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_default_argument.

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

static_assert (!has_default_argument (null_reflection));
static_assert (!has_default_argument (^^::));
static_assert (!has_default_argument (^^ns));
static_assert (!has_default_argument (^^ns_alias));
static_assert (!has_default_argument (reflect_constant (3)));
static_assert (!has_default_argument (^^cls));
static_assert (!has_default_argument (^^cls::dm));
static_assert (!has_default_argument (^^cls::ref_dm));
static_assert (!has_default_argument (^^cls::static_dm));
static_assert (!has_default_argument (^^cls::mem_fun));
static_assert (!has_default_argument (^^cls::static_mem_fun));
static_assert (!has_default_argument (^^cls::type));
static_assert (!has_default_argument (^^cls_var));
static_assert (!has_default_argument (^^onion));
static_assert (!has_default_argument (^^anon));
static_assert (!has_default_argument (^^fun));
static_assert (!has_default_argument (^^alias));
static_assert (!has_default_argument (^^var));
static_assert (!has_default_argument (^^ref));
static_assert (!has_default_argument (^^rref));
static_assert (!has_default_argument (^^ptr));
static_assert (!has_default_argument (^^cls_tmpl));
static_assert (!has_default_argument (^^cls_tmpl<int>));
static_assert (!has_default_argument (^^incomplete_cls<int>));
static_assert (!has_default_argument (^^fun_tmpl));
static_assert (!has_default_argument (^^fun_tmpl<int>));
static_assert (!has_default_argument (^^conc));
static_assert (!has_default_argument (substitute (^^conc, { ^^int })));
static_assert (!has_default_argument (^^var_tmpl));
static_assert (!has_default_argument (^^var_tmpl<int>));
static_assert (!has_default_argument (^^cls_tmpl_alias));
static_assert (!has_default_argument (^^cls_tmpl_alias<int>));
static_assert (!has_default_argument (^^Enum));
static_assert (!has_default_argument (^^Enum::A));
static_assert (!has_default_argument (^^Enum_class));
static_assert (!has_default_argument (^^Enum_class::A));
static_assert (!has_default_argument (^^decomp));
static_assert (!has_default_argument (^^decomp_ref));
static_assert (!has_default_argument (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "n" });
static_assert (!has_default_argument (dms));

struct Base {};
struct Derived : Base {};
static_assert (!has_default_argument (bases_of (^^Derived, access_context::unchecked ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_default_argument (^^T));
  static_assert (!has_default_argument (R));
  static_assert (!has_default_argument (R2));
  static_assert (!has_default_argument (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^p, ^^c>();
  static_assert (!has_default_argument (^^p));
  static_assert (!has_default_argument (^^c));
}

void h (int a, int b);
static_assert (!has_default_argument (parameters_of (^^h)[0]));
static_assert (!has_default_argument (parameters_of (^^h)[1]));
constexpr auto ha = parameters_of (^^h)[0];
constexpr auto hb = parameters_of (^^h)[1];
void h (int c, int d = 42);
static_assert (!has_default_argument (parameters_of (^^h)[0]));
static_assert (has_default_argument (parameters_of (^^h)[1]));
static_assert (!has_default_argument (ha));
static_assert (has_default_argument (hb));
void h (int c = 5, int d);
static_assert (has_default_argument (parameters_of (^^h)[0]));
static_assert (has_default_argument (parameters_of (^^h)[1]));
static_assert (has_default_argument (ha));
static_assert (has_default_argument (hb));

void
h (int e, int f)
{
  static_assert (has_default_argument (parameters_of (^^h)[0]));
  static_assert (has_default_argument (parameters_of (^^h)[1]));
  static_assert (has_default_argument (ha));
  static_assert (has_default_argument (hb));
  static_assert (!has_default_argument (^^e));
  static_assert (!has_default_argument (^^f));
}
