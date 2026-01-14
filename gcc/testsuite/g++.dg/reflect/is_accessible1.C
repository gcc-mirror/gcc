// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_accessible.

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

constexpr access_context gctx = access_context::current ();

static_assert (is_accessible (null_reflection, gctx));
static_assert (is_accessible (^^::, gctx));
static_assert (is_accessible (^^ns, gctx));
static_assert (is_accessible (^^ns_alias, gctx));
static_assert (is_accessible (reflect_constant (3), gctx));
static_assert (is_accessible (^^cls, gctx));
static_assert (is_accessible (^^cls::dm, gctx));
static_assert (is_accessible (^^cls::ref_dm, gctx));
static_assert (is_accessible (^^cls::static_dm, gctx));
static_assert (is_accessible (^^cls::mem_fun, gctx));
static_assert (is_accessible (^^cls::static_mem_fun, gctx));
static_assert (is_accessible (^^cls::type, gctx));
static_assert (is_accessible (^^cls::E, gctx));
static_assert (is_accessible (^^cls::B, gctx));
static_assert (is_accessible (^^cls::C, gctx));
static_assert (is_accessible (^^cls::D, gctx));
static_assert (is_accessible (^^cls::F, gctx));
static_assert (is_accessible (^^cls::F::G, gctx));
static_assert (is_accessible (^^cls::F::H, gctx));
static_assert (is_accessible (^^cls::S, gctx));
static_assert (is_accessible (^^cls::U, gctx));
static_assert (is_accessible (^^cls::foo, gctx));
static_assert (is_accessible (^^cls::foo <0>, gctx));
static_assert (is_accessible (^^cls::bar, gctx));
static_assert (is_accessible (^^cls::bar <42>, gctx));
static_assert (is_accessible (^^cls_var, gctx));
static_assert (is_accessible (^^onion, gctx));
static_assert (is_accessible (^^anon, gctx));
static_assert (is_accessible (^^fun, gctx));
static_assert (is_accessible (^^alias, gctx));
static_assert (is_accessible (^^var, gctx));
static_assert (is_accessible (^^ref, gctx));
static_assert (is_accessible (^^rref, gctx));
static_assert (is_accessible (^^ptr, gctx));
static_assert (is_accessible (^^cls_tmpl, gctx));
static_assert (is_accessible (^^cls_tmpl<int>, gctx));
static_assert (is_accessible (^^incomplete_cls<int>, gctx));
static_assert (is_accessible (^^fun_tmpl, gctx));
static_assert (is_accessible (^^fun_tmpl<int>, gctx));
static_assert (is_accessible (^^conc, gctx));
static_assert (is_accessible (substitute (^^conc, { ^^int }), gctx));
static_assert (is_accessible (^^var_tmpl, gctx));
static_assert (is_accessible (^^var_tmpl<int>, gctx));
static_assert (is_accessible (^^cls_tmpl_alias, gctx));
static_assert (is_accessible (^^cls_tmpl_alias<int>, gctx));
static_assert (is_accessible (^^Enum, gctx));
static_assert (is_accessible (^^Enum::A, gctx));
static_assert (is_accessible (^^Enum_class, gctx));
static_assert (is_accessible (^^Enum_class::A, gctx));
static_assert (is_accessible (^^decomp, gctx));
static_assert (is_accessible (^^decomp_ref, gctx));
static_assert (is_accessible (^^arr, gctx));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (is_accessible (dms, gctx));

struct Base {};
struct Derived : Base {};
struct Derived2 : private Base {};
static_assert (is_accessible (bases_of (^^Derived, gctx)[0], gctx));
static_assert (!is_accessible (bases_of (^^Derived2, access_context::unchecked ())[0], gctx));

void
foo (int x)
{
  static_assert (is_accessible (^^x, gctx));
  static_assert (is_accessible (parameters_of (^^foo)[0], gctx));
  int v;
  static_assert (is_accessible (^^v, gctx));
  struct S {};
  enum E { F, G };
  static_assert (is_accessible (^^S, gctx));
  static_assert (is_accessible (^^E, gctx));
  static_assert (is_accessible (^^F, gctx));
  static_assert (is_accessible (^^G, gctx));
}
