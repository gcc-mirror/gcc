// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_user_provided.

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

static_assert (!is_user_provided (null_reflection));
static_assert (!is_user_provided (^^::));
static_assert (!is_user_provided (^^ns));
static_assert (!is_user_provided (^^ns_alias));
static_assert (!is_user_provided (reflect_constant (3)));
static_assert (!is_user_provided (^^cls));
static_assert (!is_user_provided (^^cls::dm));
static_assert (!is_user_provided (^^cls::ref_dm));
static_assert (!is_user_provided (^^cls::static_dm));
static_assert (is_user_provided (^^cls::mem_fun));
static_assert (is_user_provided (^^cls::static_mem_fun));
static_assert (!is_user_provided (^^cls::type));
static_assert (!is_user_provided (^^cls::E));
static_assert (!is_user_provided (^^cls::B));
static_assert (!is_user_provided (^^cls::C));
static_assert (!is_user_provided (^^cls::D));
static_assert (!is_user_provided (^^cls::F));
static_assert (!is_user_provided (^^cls::F::G));
static_assert (!is_user_provided (^^cls::F::H));
static_assert (!is_user_provided (^^cls::S));
static_assert (!is_user_provided (^^cls::U));
static_assert (!is_user_provided (^^cls::foo));
static_assert (is_user_provided (^^cls::foo <0>));
static_assert (!is_user_provided (^^cls::bar));
static_assert (is_user_provided (^^cls::bar <42>));
static_assert (!is_user_provided (^^cls_var));
static_assert (!is_user_provided (^^onion));
static_assert (!is_user_provided (^^anon));
static_assert (is_user_provided (^^fun));
static_assert (!is_user_provided (^^alias));
static_assert (!is_user_provided (^^var));
static_assert (!is_user_provided (^^ref));
static_assert (!is_user_provided (^^rref));
static_assert (!is_user_provided (^^ptr));
static_assert (!is_user_provided (^^cls_tmpl));
static_assert (!is_user_provided (^^cls_tmpl<int>));
static_assert (!is_user_provided (^^incomplete_cls<int>));
static_assert (!is_user_provided (^^fun_tmpl));
static_assert (is_user_provided (^^fun_tmpl<int>));
static_assert (!is_user_provided (^^conc));
static_assert (!is_user_provided (substitute (^^conc, { ^^int })));
static_assert (!is_user_provided (^^var_tmpl));
static_assert (!is_user_provided (^^var_tmpl<int>));
static_assert (!is_user_provided (^^cls_tmpl_alias));
static_assert (!is_user_provided (^^cls_tmpl_alias<int>));
static_assert (!is_user_provided (^^Enum));
static_assert (!is_user_provided (^^Enum::A));
static_assert (!is_user_provided (^^Enum_class));
static_assert (!is_user_provided (^^Enum_class::A));
static_assert (!is_user_provided (^^decomp));
static_assert (!is_user_provided (^^decomp_ref));
static_assert (!is_user_provided (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_user_provided (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_user_provided (bases_of (^^Derived, access_context::current ())[0]));

void
foo (int x)
{
  static_assert (!is_user_provided (^^x));
  static_assert (!is_user_provided (parameters_of (^^foo)[0]));
  int v;
  static_assert (!is_user_provided (^^v));
  struct S {};
  enum E { F, G };
  static_assert (!is_user_provided (^^S));
  static_assert (!is_user_provided (^^E));
  static_assert (!is_user_provided (^^F));
  static_assert (!is_user_provided (^^G));
}

void bar (int, long) = delete;
void baz (int) = delete ("foobar");
static_assert (!is_user_provided (^^bar));
static_assert (!is_user_provided (^^baz));

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

static_assert (!is_user_provided (^^S::operator ==));
static_assert (!is_user_provided (^^S::operator <=>));
static_assert (!is_user_provided (^^S::operator =));
static_assert (is_user_provided (^^S::foo));
static_assert (!is_user_provided (^^S::bar));
static_assert (is_user_provided (^^S::bar <42>));
static_assert (!is_user_provided (^^V::operator =));
static_assert (is_user_provided (^^W::operator ==));
static_assert (is_user_provided (^^W::operator <=>));
static_assert (is_user_provided (^^W::operator =));
static_assert (!is_user_provided (^^X::operator ==));
static_assert (!is_user_provided (^^X::operator <=>));
static_assert (!is_user_provided (^^X::operator =));
static_assert (!is_user_provided (^^X::foo));
static_assert (!is_user_provided (^^X::bar));

struct Y
{
  ~Y () = default;
  int y;
};
static_assert (!is_user_provided (^^Y::~Y));

struct Z
{
  int z;
};
static_assert (!is_user_provided (^^Z::~Z));

struct R
{
  ~R ();
  int r;
};
static_assert (is_user_provided (^^R::~R));

struct H
{
  ~H () = delete;
  int h;
};
static_assert (!is_user_provided (^^H::~H));
