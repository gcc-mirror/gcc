// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_class_member.

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

static_assert (!is_class_member (null_reflection));
static_assert (!is_class_member (^^::));
static_assert (!is_class_member (^^ns));
static_assert (!is_class_member (^^ns_alias));
static_assert (!is_class_member (reflect_constant (3)));
static_assert (!is_class_member (^^cls));
static_assert (is_class_member (^^cls::~cls));
static_assert (is_class_member (^^cls::dm));
static_assert (is_class_member (^^cls::ref_dm));
static_assert (is_class_member (^^cls::static_dm));
static_assert (is_class_member (^^cls::mem_fun));
static_assert (is_class_member (^^cls::static_mem_fun));
static_assert (is_class_member (^^cls::type));
static_assert (is_class_member (^^cls::E));
static_assert (is_class_member (^^cls::B));
static_assert (is_class_member (^^cls::C));
static_assert (is_class_member (^^cls::D));
static_assert (is_class_member (^^cls::F));
static_assert (!is_class_member (^^cls::F::G));
static_assert (!is_class_member (^^cls::F::H));
static_assert (is_class_member (^^cls::S));
static_assert (is_class_member (^^cls::U));
static_assert (is_class_member (^^cls::foo));
static_assert (is_class_member (^^cls::foo <0>));
static_assert (is_class_member (^^cls::bar));
static_assert (is_class_member (^^cls::bar <42>));
static_assert (!is_class_member (^^cls_var));
static_assert (!is_class_member (^^onion));
static_assert (is_class_member (^^anon));
static_assert (!is_class_member (^^fun));
static_assert (!is_class_member (^^alias));
static_assert (!is_class_member (^^var));
static_assert (!is_class_member (^^ref));
static_assert (!is_class_member (^^rref));
static_assert (!is_class_member (^^ptr));
static_assert (!is_class_member (^^cls_tmpl));
static_assert (!is_class_member (^^cls_tmpl<int>));
static_assert (!is_class_member (^^incomplete_cls<int>));
static_assert (!is_class_member (^^fun_tmpl));
static_assert (!is_class_member (^^fun_tmpl<int>));
static_assert (!is_class_member (^^conc));
static_assert (!is_class_member (substitute (^^conc, { ^^int })));
static_assert (!is_class_member (^^var_tmpl));
static_assert (!is_class_member (^^var_tmpl<int>));
static_assert (!is_class_member (^^cls_tmpl_alias));
static_assert (!is_class_member (^^cls_tmpl_alias<int>));
static_assert (!is_class_member (^^Enum));
static_assert (!is_class_member (^^Enum::A));
static_assert (!is_class_member (^^Enum_class));
static_assert (!is_class_member (^^Enum_class::A));
static_assert (!is_class_member (^^decomp));
static_assert (!is_class_member (^^decomp_ref));
static_assert (!is_class_member (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_class_member (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_class_member (bases_of (^^Derived, access_context::current ())[0]));

void
foo (int x)
{
  static_assert (!is_class_member (^^x));
  static_assert (!is_class_member (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_class_member (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_class_member (^^S));
  static_assert (!is_class_member (^^E));
  static_assert (!is_class_member (^^F));
  static_assert (!is_class_member (^^G));
}
