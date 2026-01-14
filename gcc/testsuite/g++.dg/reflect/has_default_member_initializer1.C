// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_default_member_initializer.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
struct cls {
  int dm;
  static int static_dm;
  void mem_fun ();
  static void static_mem_fun ();
  int &ref_dm = dm;
  int dm2 = 42;
  int &ref_dm2;
  using type = int;
  enum E {
    B, C, D
  };
  enum class F {
    G, H
  };
  struct S {};
  union U {};
  template <int N>
  void foo ();
  template <int N>
  static void bar ();
  cls () : ref_dm2 (dm2) {}
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

static_assert (!has_default_member_initializer (null_reflection));
static_assert (!has_default_member_initializer (^^::));
static_assert (!has_default_member_initializer (^^ns));
static_assert (!has_default_member_initializer (^^ns_alias));
static_assert (!has_default_member_initializer (reflect_constant (3)));
static_assert (!has_default_member_initializer (^^cls));
static_assert (!has_default_member_initializer (^^cls::dm));
static_assert (has_default_member_initializer (^^cls::ref_dm));
static_assert (has_default_member_initializer (^^cls::dm2));
static_assert (!has_default_member_initializer (^^cls::ref_dm2));
static_assert (!has_default_member_initializer (^^cls::static_dm));
static_assert (!has_default_member_initializer (^^cls::mem_fun));
static_assert (!has_default_member_initializer (^^cls::static_mem_fun));
static_assert (!has_default_member_initializer (^^cls::type));
static_assert (!has_default_member_initializer (^^cls::E));
static_assert (!has_default_member_initializer (^^cls::B));
static_assert (!has_default_member_initializer (^^cls::C));
static_assert (!has_default_member_initializer (^^cls::D));
static_assert (!has_default_member_initializer (^^cls::F));
static_assert (!has_default_member_initializer (^^cls::F::G));
static_assert (!has_default_member_initializer (^^cls::F::H));
static_assert (!has_default_member_initializer (^^cls::S));
static_assert (!has_default_member_initializer (^^cls::U));
static_assert (!has_default_member_initializer (^^cls::foo));
static_assert (!has_default_member_initializer (^^cls::foo <0>));
static_assert (!has_default_member_initializer (^^cls::bar));
static_assert (!has_default_member_initializer (^^cls::bar <42>));
static_assert (!has_default_member_initializer (^^cls_var));
static_assert (!has_default_member_initializer (^^onion));
static_assert (!has_default_member_initializer (^^anon));
static_assert (!has_default_member_initializer (^^fun));
static_assert (!has_default_member_initializer (^^alias));
static_assert (!has_default_member_initializer (^^var));
static_assert (!has_default_member_initializer (^^ref));
static_assert (!has_default_member_initializer (^^rref));
static_assert (!has_default_member_initializer (^^ptr));
static_assert (!has_default_member_initializer (^^cls_tmpl));
static_assert (!has_default_member_initializer (^^cls_tmpl<int>));
static_assert (!has_default_member_initializer (^^incomplete_cls<int>));
static_assert (!has_default_member_initializer (^^fun_tmpl));
static_assert (!has_default_member_initializer (^^fun_tmpl<int>));
static_assert (!has_default_member_initializer (^^conc));
static_assert (!has_default_member_initializer (substitute (^^conc, { ^^int })));
static_assert (!has_default_member_initializer (^^var_tmpl));
static_assert (!has_default_member_initializer (^^var_tmpl<int>));
static_assert (!has_default_member_initializer (^^cls_tmpl_alias));
static_assert (!has_default_member_initializer (^^cls_tmpl_alias<int>));
static_assert (!has_default_member_initializer (^^Enum));
static_assert (!has_default_member_initializer (^^Enum::A));
static_assert (!has_default_member_initializer (^^Enum_class));
static_assert (!has_default_member_initializer (^^Enum_class::A));
static_assert (!has_default_member_initializer (^^decomp));
static_assert (!has_default_member_initializer (^^decomp_ref));
static_assert (!has_default_member_initializer (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "a" });
static_assert (!has_default_member_initializer (dms));

struct Base {};
struct Derived : Base {};
static_assert (!has_default_member_initializer (bases_of (^^Derived, access_context::unchecked ())[0]));

void
foo (int x, int y = 10)
{
  static_assert (!has_default_member_initializer (^^x));
  static_assert (!has_default_member_initializer (parameters_of (^^foo)[0]));
  static_assert (!has_default_member_initializer (parameters_of (^^foo)[1]));
  int v;
  static_assert (!has_default_member_initializer (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!has_default_member_initializer (^^S));
  static_assert (!has_default_member_initializer (^^E));
  static_assert (!has_default_member_initializer (^^F));
  static_assert (!has_default_member_initializer (^^G));
}
