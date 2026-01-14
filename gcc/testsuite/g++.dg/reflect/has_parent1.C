// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_parent.

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

static_assert (!has_parent (null_reflection));
static_assert (!has_parent (^^::));
static_assert (has_parent (^^ns));
static_assert (has_parent (^^ns_alias));
static_assert (!has_parent (reflect_constant (3)));
static_assert (has_parent (^^cls));
static_assert (has_parent (^^cls::dm));
static_assert (has_parent (^^cls::ref_dm));
static_assert (has_parent (^^cls::static_dm));
static_assert (has_parent (^^cls::mem_fun));
static_assert (has_parent (^^cls::static_mem_fun));
static_assert (has_parent (^^cls::type));
static_assert (has_parent (^^cls_var));
static_assert (has_parent (^^onion));
static_assert (has_parent (^^anon));
static_assert (has_parent (^^fun));
static_assert (has_parent (^^alias));
static_assert (has_parent (^^var));
static_assert (has_parent (^^ref));
static_assert (has_parent (^^rref));
static_assert (has_parent (^^ptr));
static_assert (has_parent (^^cls_tmpl));
static_assert (has_parent (^^cls_tmpl<int>));
static_assert (has_parent (^^incomplete_cls<int>));
static_assert (has_parent (^^fun_tmpl));
static_assert (has_parent (^^fun_tmpl<int>));
static_assert (has_parent (^^conc));
static_assert (!has_parent (substitute (^^conc, { ^^int })));
static_assert (has_parent (^^var_tmpl));
static_assert (has_parent (^^var_tmpl<int>));
static_assert (has_parent (^^cls_tmpl_alias));
static_assert (has_parent (^^cls_tmpl_alias<int>));
static_assert (has_parent (^^Enum));
static_assert (has_parent (^^Enum::A));
static_assert (has_parent (^^Enum_class));
static_assert (has_parent (^^Enum_class::A));
static_assert (has_parent (^^decomp));
static_assert (has_parent (^^decomp_ref));
static_assert (has_parent (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!has_parent (dms));

struct Base {};
struct Derived : Base {};
static_assert (has_parent (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_parent (^^T));
  static_assert (has_parent (R));
  static_assert (has_parent (R2));
  static_assert (has_parent (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (has_parent (^^p));
  static_assert (has_parent (^^c));
  static_assert (has_parent (parameters_of (^^g)[0]));
}

int
main ()
{
}

namespace N
{
  extern "C" void foo (int);
  void baz (int);
}
extern "C" void bar ();

static_assert (!has_parent (^^main));
static_assert (!has_parent (^^N::foo));
static_assert (!has_parent (^^bar));
static_assert (has_parent (^^N::baz));

enum F {
  F1,
  F2 = has_parent (^^F1),
  F3 = has_parent (^^F2) + 1,
};
static_assert (F2 == 1 && F3 == 2);

void
qux ()
{
  auto a = [] () { int b; static_assert (has_parent (^^b)); };
  static_assert (has_parent (^^a));
  static_assert (has_parent (type_of (^^a)));
}
