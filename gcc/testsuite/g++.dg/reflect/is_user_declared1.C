// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_user_declared.

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

static_assert (!is_user_declared (null_reflection));
static_assert (!is_user_declared (^^::));
static_assert (!is_user_declared (^^ns));
static_assert (!is_user_declared (^^ns_alias));
static_assert (!is_user_declared (reflect_constant (3)));
static_assert (!is_user_declared (^^cls));
static_assert (!is_user_declared (^^cls::dm));
static_assert (!is_user_declared (^^cls::ref_dm));
static_assert (!is_user_declared (^^cls::static_dm));
static_assert (is_user_declared (^^cls::mem_fun));
static_assert (is_user_declared (^^cls::static_mem_fun));
static_assert (!is_user_declared (^^cls::type));
static_assert (!is_user_declared (^^cls::E));
static_assert (!is_user_declared (^^cls::B));
static_assert (!is_user_declared (^^cls::C));
static_assert (!is_user_declared (^^cls::D));
static_assert (!is_user_declared (^^cls::F));
static_assert (!is_user_declared (^^cls::F::G));
static_assert (!is_user_declared (^^cls::F::H));
static_assert (!is_user_declared (^^cls::S));
static_assert (!is_user_declared (^^cls::U));
static_assert (!is_user_declared (^^cls::foo));
static_assert (is_user_declared (^^cls::foo <0>));
static_assert (!is_user_declared (^^cls::bar));
static_assert (is_user_declared (^^cls::bar <42>));
static_assert (!is_user_declared (^^cls_var));
static_assert (!is_user_declared (^^onion));
static_assert (!is_user_declared (^^anon));
static_assert (is_user_declared (^^fun));
static_assert (!is_user_declared (^^alias));
static_assert (!is_user_declared (^^var));
static_assert (!is_user_declared (^^ref));
static_assert (!is_user_declared (^^rref));
static_assert (!is_user_declared (^^ptr));
static_assert (!is_user_declared (^^cls_tmpl));
static_assert (!is_user_declared (^^cls_tmpl<int>));
static_assert (!is_user_declared (^^incomplete_cls<int>));
static_assert (!is_user_declared (^^fun_tmpl));
static_assert (is_user_declared (^^fun_tmpl<int>));
static_assert (!is_user_declared (^^conc));
static_assert (!is_user_declared (substitute (^^conc, { ^^int })));
static_assert (!is_user_declared (^^var_tmpl));
static_assert (!is_user_declared (^^var_tmpl<int>));
static_assert (!is_user_declared (^^cls_tmpl_alias));
static_assert (!is_user_declared (^^cls_tmpl_alias<int>));
static_assert (!is_user_declared (^^Enum));
static_assert (!is_user_declared (^^Enum::A));
static_assert (!is_user_declared (^^Enum_class));
static_assert (!is_user_declared (^^Enum_class::A));
static_assert (!is_user_declared (^^decomp));
static_assert (!is_user_declared (^^decomp_ref));
static_assert (!is_user_declared (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_user_declared (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_user_declared (bases_of (^^Derived, access_context::current ())[0]));

void
foo (int x)
{
  static_assert (!is_user_declared (^^x));
  static_assert (!is_user_declared (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_user_declared (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_user_declared (^^S));
  static_assert (!is_user_declared (^^E));
  static_assert (!is_user_declared (^^F));
  static_assert (!is_user_declared (^^G));
}

void bar (int, long) = delete;
void baz (int) = delete ("foobar");
static_assert (is_user_declared (^^bar));
static_assert (is_user_declared (^^baz));

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

struct W
{
  W (const W &);
  bool operator == (const W &) const;
  auto operator <=> (const W &) const;
  W &operator = (const W &) { return *this; }
};

struct X
{
  X (const X &);
  bool operator == (const X &) const = delete;
  auto operator <=> (const X &) const = delete;
  X &operator = (const X &) = delete;
  void foo () = delete;
  template <int N>
  void bar () = delete;
};

static_assert (is_user_declared (^^S::operator ==));
static_assert (is_user_declared (^^S::operator <=>));
static_assert (is_user_declared (^^S::operator =));
static_assert (is_user_declared (^^S::foo));
static_assert (!is_user_declared (^^S::bar));
static_assert (is_user_declared (^^S::bar <42>));
static_assert (!is_user_declared (^^V::operator =));
static_assert (is_user_declared (^^W::operator ==));
static_assert (is_user_declared (^^W::operator <=>));
static_assert (is_user_declared (^^W::operator =));
static_assert (is_user_declared (^^X::operator ==));
static_assert (is_user_declared (^^X::operator <=>));
static_assert (is_user_declared (^^X::operator =));
static_assert (is_user_declared (^^X::foo));
static_assert (!is_user_declared (^^X::bar));

struct Y
{
  ~Y () = default;
  int y;
};
static_assert (is_user_declared (^^Y::~Y));

struct Z
{
  int z;
};
static_assert (!is_user_declared (^^Z::~Z));

struct R
{
  ~R ();
  int r;
};
static_assert (is_user_declared (^^R::~R));

struct H
{
  ~H () = delete;
  int h;
};
static_assert (is_user_declared (^^H::~H));
