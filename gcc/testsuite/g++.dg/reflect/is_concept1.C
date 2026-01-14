// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_concept.

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

static_assert (!is_concept (null_reflection));
static_assert (!is_concept (^^::));
static_assert (!is_concept (^^ns));
static_assert (!is_concept (^^ns_alias));
static_assert (!is_concept (reflect_constant (3)));
static_assert (!is_concept (^^cls));
static_assert (!is_concept (^^cls::dm));
static_assert (!is_concept (^^cls::ref_dm));
static_assert (!is_concept (^^cls::static_dm));
static_assert (!is_concept (^^cls::mem_fun));
static_assert (!is_concept (^^cls::static_mem_fun));
static_assert (!is_concept (^^cls::type));
static_assert (!is_concept (^^cls_var));
static_assert (!is_concept (^^onion));
static_assert (!is_concept (^^anon));
static_assert (!is_concept (^^fun));
static_assert (!is_concept (^^alias));
static_assert (!is_concept (^^var));
static_assert (!is_concept (^^ref));
static_assert (!is_concept (^^rref));
static_assert (!is_concept (^^ptr));
static_assert (!is_concept (^^cls_tmpl));
static_assert (!is_concept (^^cls_tmpl<int>));
static_assert (!is_concept (^^incomplete_cls<int>));
static_assert (!is_concept (^^fun_tmpl));
static_assert (!is_concept (^^fun_tmpl<int>));
static_assert (is_concept (^^conc));
static_assert (!is_concept (substitute (^^conc, { ^^int })));
static_assert (!is_concept (^^var_tmpl));
static_assert (!is_concept (^^var_tmpl<int>));
static_assert (!is_concept (^^cls_tmpl_alias));
static_assert (!is_concept (^^cls_tmpl_alias<int>));
static_assert (!is_concept (^^Enum));
static_assert (!is_concept (^^Enum::A));
static_assert (!is_concept (^^Enum_class));
static_assert (!is_concept (^^Enum_class::A));
static_assert (!is_concept (^^decomp));
static_assert (!is_concept (^^decomp_ref));
static_assert (!is_concept (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_concept (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_concept (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_concept (^^T));
  static_assert (!is_concept (R));
  static_assert (!is_concept (R2));
  static_assert (is_concept (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^conc>();
  static_assert (!is_concept (^^p));
  static_assert (!is_concept (^^c));
}
