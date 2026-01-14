// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_defaulted.

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

static_assert (!is_defaulted (null_reflection));
static_assert (!is_defaulted (^^::));
static_assert (!is_defaulted (^^ns));
static_assert (!is_defaulted (^^ns_alias));
static_assert (!is_defaulted (reflect_constant (3)));
static_assert (!is_defaulted (^^cls));
static_assert (!is_defaulted (^^cls::dm));
static_assert (!is_defaulted (^^cls::ref_dm));
static_assert (!is_defaulted (^^cls::static_dm));
static_assert (!is_defaulted (^^cls::mem_fun));
static_assert (!is_defaulted (^^cls::static_mem_fun));
static_assert (!is_defaulted (^^cls::type));
static_assert (!is_defaulted (^^cls::E));
static_assert (!is_defaulted (^^cls::B));
static_assert (!is_defaulted (^^cls::C));
static_assert (!is_defaulted (^^cls::D));
static_assert (!is_defaulted (^^cls::F));
static_assert (!is_defaulted (^^cls::F::G));
static_assert (!is_defaulted (^^cls::F::H));
static_assert (!is_defaulted (^^cls::S));
static_assert (!is_defaulted (^^cls::U));
static_assert (!is_defaulted (^^cls::foo));
static_assert (!is_defaulted (^^cls::foo <0>));
static_assert (!is_defaulted (^^cls::bar));
static_assert (!is_defaulted (^^cls::bar <42>));
static_assert (!is_defaulted (^^cls_var));
static_assert (!is_defaulted (^^onion));
static_assert (!is_defaulted (^^anon));
static_assert (!is_defaulted (^^fun));
static_assert (!is_defaulted (^^alias));
static_assert (!is_defaulted (^^var));
static_assert (!is_defaulted (^^ref));
static_assert (!is_defaulted (^^rref));
static_assert (!is_defaulted (^^ptr));
static_assert (!is_defaulted (^^cls_tmpl));
static_assert (!is_defaulted (^^cls_tmpl<int>));
static_assert (!is_defaulted (^^incomplete_cls<int>));
static_assert (!is_defaulted (^^fun_tmpl));
static_assert (!is_defaulted (^^fun_tmpl<int>));
static_assert (!is_defaulted (^^conc));
static_assert (!is_defaulted (substitute (^^conc, { ^^int })));
static_assert (!is_defaulted (^^var_tmpl));
static_assert (!is_defaulted (^^var_tmpl<int>));
static_assert (!is_defaulted (^^cls_tmpl_alias));
static_assert (!is_defaulted (^^cls_tmpl_alias<int>));
static_assert (!is_defaulted (^^Enum));
static_assert (!is_defaulted (^^Enum::A));
static_assert (!is_defaulted (^^Enum_class));
static_assert (!is_defaulted (^^Enum_class::A));
static_assert (!is_defaulted (^^decomp));
static_assert (!is_defaulted (^^decomp_ref));
static_assert (!is_defaulted (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_defaulted (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_defaulted (bases_of (^^Derived, access_context::current ())[0]));

void
foo (int x)
{
  static_assert (!is_defaulted (^^x));
  static_assert (!is_defaulted (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_defaulted (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_defaulted (^^S));
  static_assert (!is_defaulted (^^E));
  static_assert (!is_defaulted (^^F));
  static_assert (!is_defaulted (^^G));
}

void bar (int, long) = delete;
void baz (int) = delete ("foobar");
static_assert (!is_defaulted (^^bar));
static_assert (!is_defaulted (^^baz));

struct S
{
  bool operator == (const S &) const = default;
  auto operator <=> (const S &) const = default;
  S &operator = (const S &) = default;
  int s;
  void foo ();
  template <int N>
  void bar ();
};

struct V
{
  V (const V &);
};

static_assert (is_defaulted (^^S::operator <=>));
static_assert (is_defaulted (^^S::operator <=>));
static_assert (is_defaulted (^^S::operator =));
static_assert (!is_defaulted (^^S::foo));
static_assert (!is_defaulted (^^S::bar));
static_assert (!is_defaulted (^^S::bar <42>));
static_assert (is_defaulted (^^V::operator =));

struct W
{
  ~W () = default;
  int w;
};
static_assert (is_defaulted (^^W::~W));

struct X
{
  int x;
};
static_assert (is_defaulted (^^X::~X));
