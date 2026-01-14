// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_internal_linkage.

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

static_assert (!has_internal_linkage (null_reflection));
static_assert (!has_internal_linkage (^^::));
static_assert (!has_internal_linkage (^^ns));
static_assert (!has_internal_linkage (^^std));
static_assert (!has_internal_linkage (^^std::meta));
static_assert (!has_internal_linkage (^^ns_alias));
static_assert (!has_internal_linkage (reflect_constant (3)));
static_assert (!has_internal_linkage (^^cls));
static_assert (!has_internal_linkage (^^cls::dm));
static_assert (!has_internal_linkage (^^cls::ref_dm));
static_assert (!has_internal_linkage (^^cls::static_dm));
static_assert (!has_internal_linkage (^^cls::mem_fun));
static_assert (!has_internal_linkage (^^cls::static_mem_fun));
static_assert (!has_internal_linkage (^^cls::type));
static_assert (!has_internal_linkage (^^cls_var));
static_assert (!has_internal_linkage (^^onion));
static_assert (!has_internal_linkage (^^anon));
static_assert (!has_internal_linkage (^^fun));
static_assert (!has_internal_linkage (^^alias));
static_assert (!has_internal_linkage (^^var));
static_assert (!has_internal_linkage (^^ref));
static_assert (!has_internal_linkage (^^rref));
static_assert (!has_internal_linkage (^^ptr));
static_assert (!has_internal_linkage (^^cls_tmpl));
static_assert (!has_internal_linkage (^^cls_tmpl<int>));
static_assert (!has_internal_linkage (^^incomplete_cls<int>));
static_assert (!has_internal_linkage (^^fun_tmpl));
static_assert (!has_internal_linkage (^^fun_tmpl<int>));
static_assert (!has_internal_linkage (^^conc));
static_assert (!has_internal_linkage (substitute (^^conc, { ^^int })));
static_assert (!has_internal_linkage (^^var_tmpl));
static_assert (!has_internal_linkage (^^var_tmpl<int>));
static_assert (!has_internal_linkage (^^cls_tmpl_alias));
static_assert (!has_internal_linkage (^^cls_tmpl_alias<int>));
static_assert (!has_internal_linkage (^^Enum));
static_assert (!has_internal_linkage (^^Enum::A));
static_assert (!has_internal_linkage (^^Enum_class));
static_assert (!has_internal_linkage (^^Enum_class::A));
static_assert (!has_internal_linkage (^^decomp));
static_assert (!has_internal_linkage (^^decomp_ref));
static_assert (!has_internal_linkage (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!has_internal_linkage (dms));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_internal_linkage (^^T));
  static_assert (!has_internal_linkage (R));
  static_assert (!has_internal_linkage (R2));
  static_assert (!has_internal_linkage (R3));
}

void
g (int p, cls c)
{
  int v;
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!has_internal_linkage (^^p));
  static_assert (!has_internal_linkage (^^c));
  static_assert (!has_internal_linkage (^^v));
}

static int var2;
static int baz () { return 0; }

static_assert (has_internal_linkage (^^var2));
static_assert (has_internal_linkage (^^baz));

namespace {
  void qux () {}
  int var3;
  namespace NN {
    int var4;
  }
  struct S2 {};
}

namespace NM {
  int var5;
  struct S3 {};
}

extern "C" {
  void corge (int);
}

int
main ()
{
}

const int ci = 42;
extern const int ci2 = 42;

static_assert (has_internal_linkage (^^qux));
static_assert (has_internal_linkage (^^var3));
static_assert (has_internal_linkage (^^NN));
static_assert (has_internal_linkage (^^NN::var4));
static_assert (!has_internal_linkage (^^NM::var5));
static_assert (has_internal_linkage (^^S2));
static_assert (!has_internal_linkage (^^NM::S3));
static_assert (!has_internal_linkage (^^corge));
static_assert (!has_internal_linkage (^^main));
static_assert (has_internal_linkage (^^ci));
static_assert (!has_internal_linkage (^^ci2));
