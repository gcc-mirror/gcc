// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_deleted.

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

static_assert (!is_deleted (null_reflection));
static_assert (!is_deleted (^^::));
static_assert (!is_deleted (^^ns));
static_assert (!is_deleted (^^ns_alias));
static_assert (!is_deleted (reflect_constant (3)));
static_assert (!is_deleted (^^cls));
static_assert (!is_deleted (^^cls::dm));
static_assert (!is_deleted (^^cls::ref_dm));
static_assert (!is_deleted (^^cls::static_dm));
static_assert (!is_deleted (^^cls::mem_fun));
static_assert (!is_deleted (^^cls::static_mem_fun));
static_assert (!is_deleted (^^cls::type));
static_assert (!is_deleted (^^cls::E));
static_assert (!is_deleted (^^cls::B));
static_assert (!is_deleted (^^cls::C));
static_assert (!is_deleted (^^cls::D));
static_assert (!is_deleted (^^cls::F));
static_assert (!is_deleted (^^cls::F::G));
static_assert (!is_deleted (^^cls::F::H));
static_assert (!is_deleted (^^cls::S));
static_assert (!is_deleted (^^cls::U));
static_assert (!is_deleted (^^cls::foo));
static_assert (!is_deleted (^^cls::foo <0>));
static_assert (!is_deleted (^^cls::bar));
static_assert (!is_deleted (^^cls::bar <42>));
static_assert (!is_deleted (^^cls_var));
static_assert (!is_deleted (^^onion));
static_assert (!is_deleted (^^anon));
static_assert (!is_deleted (^^fun));
static_assert (!is_deleted (^^alias));
static_assert (!is_deleted (^^var));
static_assert (!is_deleted (^^ref));
static_assert (!is_deleted (^^rref));
static_assert (!is_deleted (^^ptr));
static_assert (!is_deleted (^^cls_tmpl));
static_assert (!is_deleted (^^cls_tmpl<int>));
static_assert (!is_deleted (^^incomplete_cls<int>));
static_assert (!is_deleted (^^fun_tmpl));
static_assert (!is_deleted (^^fun_tmpl<int>));
static_assert (!is_deleted (^^conc));
static_assert (!is_deleted (substitute (^^conc, { ^^int })));
static_assert (!is_deleted (^^var_tmpl));
static_assert (!is_deleted (^^var_tmpl<int>));
static_assert (!is_deleted (^^cls_tmpl_alias));
static_assert (!is_deleted (^^cls_tmpl_alias<int>));
static_assert (!is_deleted (^^Enum));
static_assert (!is_deleted (^^Enum::A));
static_assert (!is_deleted (^^Enum_class));
static_assert (!is_deleted (^^Enum_class::A));
static_assert (!is_deleted (^^decomp));
static_assert (!is_deleted (^^decomp_ref));
static_assert (!is_deleted (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = u8"dms" });
static_assert (!is_deleted (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_deleted (bases_of (^^Derived, access_context::unprivileged ())[0]));

void
foo (int x)
{
  static_assert (!is_deleted (^^x));
  static_assert (!is_deleted (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_deleted (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_deleted (^^S));
  static_assert (!is_deleted (^^E));
  static_assert (!is_deleted (^^F));
  static_assert (!is_deleted (^^G));
}

void bar (int, long) = delete;
void baz (int) = delete ("foobar");
static_assert (is_deleted (^^bar));
static_assert (is_deleted (^^baz));

struct S
{
  S (const int &x) : s (x) {}
  auto operator <=> (const S &) const = default;
  const int &s;
  void foo () = delete;
  template <int N>
  void bar () = delete;
};
S s1 = 42;
S s2 = 43;
auto s3 = s1 <=> s2; // { dg-error "use of deleted function" }
static_assert (is_deleted (^^S::operator <=>));
static_assert (is_deleted (^^S::foo));
static_assert (!is_deleted (^^S::bar));

struct W
{
  ~W () = delete;
};
static_assert (is_deleted (^^W::~W));
