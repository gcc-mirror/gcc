// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_bit_field.

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

static_assert (!is_bit_field (null_reflection));
static_assert (!is_bit_field (^^::));
static_assert (!is_bit_field (^^ns));
static_assert (!is_bit_field (^^ns_alias));
static_assert (!is_bit_field (reflect_constant (3)));
static_assert (!is_bit_field (^^cls));
static_assert (!is_bit_field (^^cls::dm));
static_assert (!is_bit_field (^^cls::ref_dm));
static_assert (!is_bit_field (^^cls::static_dm));
static_assert (!is_bit_field (^^cls::mem_fun));
static_assert (!is_bit_field (^^cls::static_mem_fun));
static_assert (!is_bit_field (^^cls::type));
static_assert (!is_bit_field (^^cls::E));
static_assert (!is_bit_field (^^cls::B));
static_assert (!is_bit_field (^^cls::C));
static_assert (!is_bit_field (^^cls::D));
static_assert (!is_bit_field (^^cls::F));
static_assert (!is_bit_field (^^cls::F::G));
static_assert (!is_bit_field (^^cls::F::H));
static_assert (!is_bit_field (^^cls::S));
static_assert (!is_bit_field (^^cls::U));
static_assert (!is_bit_field (^^cls::foo));
static_assert (!is_bit_field (^^cls::foo <0>));
static_assert (!is_bit_field (^^cls::bar));
static_assert (!is_bit_field (^^cls::bar <42>));
static_assert (!is_bit_field (^^cls_var));
static_assert (!is_bit_field (^^onion));
static_assert (!is_bit_field (^^anon));
static_assert (!is_bit_field (^^fun));
static_assert (!is_bit_field (^^alias));
static_assert (!is_bit_field (^^var));
static_assert (!is_bit_field (^^ref));
static_assert (!is_bit_field (^^rref));
static_assert (!is_bit_field (^^ptr));
static_assert (!is_bit_field (^^cls_tmpl));
static_assert (!is_bit_field (^^cls_tmpl<int>));
static_assert (!is_bit_field (^^incomplete_cls<int>));
static_assert (!is_bit_field (^^fun_tmpl));
static_assert (!is_bit_field (^^fun_tmpl<int>));
static_assert (!is_bit_field (^^conc));
static_assert (!is_bit_field (substitute (^^conc, { ^^int })));
static_assert (!is_bit_field (^^var_tmpl));
static_assert (!is_bit_field (^^var_tmpl<int>));
static_assert (!is_bit_field (^^cls_tmpl_alias));
static_assert (!is_bit_field (^^cls_tmpl_alias<int>));
static_assert (!is_bit_field (^^Enum));
static_assert (!is_bit_field (^^Enum::A));
static_assert (!is_bit_field (^^Enum_class));
static_assert (!is_bit_field (^^Enum_class::A));
static_assert (!is_bit_field (^^decomp));
static_assert (!is_bit_field (^^decomp_ref));
static_assert (!is_bit_field (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_bit_field (dms));
constexpr auto dms2 = data_member_spec (^^int, { .bit_width = 6 });
static_assert (is_bit_field (dms2));
constexpr auto dms3 = data_member_spec (^^int, { .name = "dms", .bit_width = 15 });
static_assert (is_bit_field (dms3));

constexpr auto ctx = access_context::current ();

struct Base {};
struct Derived : Base {};
static_assert (!is_bit_field (bases_of (^^Derived, ctx)[0]));

struct S {
  int bf : 4;
  long long bf2 : 36;
  int : 4;
  int : 0;
};
static_assert (is_bit_field (^^S::bf));
static_assert (is_bit_field (^^S::bf2));
static_assert (is_bit_field (members_of (^^S, ctx)[2]));
static_assert (is_bit_field (members_of (^^S, ctx)[3]));

void
foo (int x)
{
  static_assert (!is_bit_field (^^x));
  static_assert (!is_bit_field (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_bit_field (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_bit_field (^^S));
  static_assert (!is_bit_field (^^E));
  static_assert (!is_bit_field (^^F));
  static_assert (!is_bit_field (^^G));
}
