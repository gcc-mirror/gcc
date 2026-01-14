// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::can_substitute.

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

template <reflection_range R = std::initializer_list <info>>
consteval bool
could_substitute (info r, R &&args)
{
  try { can_substitute (r, args); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!could_substitute (null_reflection, {}));
static_assert (!could_substitute (^^::, {}));
static_assert (!could_substitute (^^ns, {}));
static_assert (!could_substitute (^^ns_alias, {}));
static_assert (!could_substitute (reflect_constant (3), {}));
static_assert (!could_substitute (^^cls, {}));
static_assert (!could_substitute (^^cls::dm, {}));
static_assert (!could_substitute (^^cls::ref_dm, {}));
static_assert (!could_substitute (^^cls::static_dm, {}));
static_assert (!could_substitute (^^cls::mem_fun, {}));
static_assert (!could_substitute (^^cls::static_mem_fun, {}));
static_assert (!could_substitute (^^cls::type, {}));
static_assert (!could_substitute (^^cls_var, {}));
static_assert (!could_substitute (^^onion, {}));
static_assert (!could_substitute (^^anon, {}));
static_assert (!could_substitute (^^fun, {}));
static_assert (!could_substitute (^^alias, {}));
static_assert (!could_substitute (^^var, {}));
static_assert (!could_substitute (^^ref, {}));
static_assert (!could_substitute (^^rref, {}));
static_assert (!could_substitute (^^ptr, {}));
static_assert (could_substitute (^^cls_tmpl, {}));
static_assert (!could_substitute (^^cls_tmpl<int>, {}));
static_assert (!could_substitute (^^incomplete_cls<int>, {}));
static_assert (could_substitute (^^fun_tmpl, {}));
static_assert (!could_substitute (^^fun_tmpl<int>, {}));
static_assert (could_substitute (^^conc, {}));
static_assert (!could_substitute (substitute (^^conc, { ^^int }), {}));
static_assert (could_substitute (^^var_tmpl, {}));
static_assert (!could_substitute (^^var_tmpl<int>, {}));
static_assert (could_substitute (^^cls_tmpl_alias, {}));
static_assert (!could_substitute (^^cls_tmpl_alias<int>, {}));
static_assert (!could_substitute (^^Enum, {}));
static_assert (!could_substitute (^^Enum::A, {}));
static_assert (!could_substitute (^^Enum_class, {}));
static_assert (!could_substitute (^^Enum_class::A, {}));
static_assert (!could_substitute (^^decomp, {}));
static_assert (!could_substitute (^^decomp_ref, {}));
static_assert (!could_substitute (^^arr, {}));

constexpr auto dms = data_member_spec (^^int, { .name = "a" });
static_assert (!could_substitute (dms, {}));

struct Base {};
struct Derived : Base {};
static_assert (!could_substitute (bases_of (^^Derived, access_context::unchecked ())[0], {}));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!could_substitute (^^T, {}));
  static_assert (!could_substitute (R, {}));
  static_assert (!could_substitute (R2, {}));
  static_assert (could_substitute (R3, {}));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls_tmpl_alias>();
  static_assert (!could_substitute (^^p, {}));
  static_assert (!could_substitute (^^c, {}));
}

template <typename T>
struct S {};

template <float F, int N>
struct T {};

template <typename ...T>
struct U {};

template <typename T>
T foo (T x) { return x; }

template <int N>
constexpr int v = N;

template <typename T>
concept C = requires { T::s; };

struct V { int s; };

struct NS {
  constexpr NS (int x) : ns (x) {}
  constexpr NS (const NS &x) : ns (x.ns) {}
  constexpr ~NS () {}
private:
  int ns;
};
[[=1]] void bar (int x);

static_assert (could_substitute (^^S, {}));
static_assert (!could_substitute (^^S, { null_reflection }));
static_assert (!could_substitute (^^S, { parameters_of (^^bar)[0] }));
static_assert (!could_substitute (^^S, { annotations_of (^^bar)[0] }));
static_assert (!could_substitute (^^S, { ^^:: }));
static_assert (!could_substitute (^^S, { ^^ns }));
static_assert (!could_substitute (^^S, { ^^ns_alias }));
static_assert (!could_substitute (^^S, { ^^NS::~NS }));
static_assert (could_substitute (^^S, { reflect_constant (42) }));
constexpr int n = 42;
static_assert (could_substitute (^^S, { ^^n }));
constexpr NS nsv (42);
static_assert (could_substitute (^^S, { ^^nsv }));

static_assert (!can_substitute (^^S, {}));
static_assert (can_substitute (^^S, { ^^int }));
static_assert (can_substitute (^^S, { ^^V }));
static_assert (can_substitute (^^S, { ^^NS }));
static_assert (!can_substitute (^^S, { ^^int, ^^long }));
static_assert (!can_substitute (^^S, { reflect_constant (42) }));
static_assert (!can_substitute (^^S, { ^^n }));
static_assert (!can_substitute (^^S, { ^^nsv }));
static_assert (!can_substitute (^^T, {}));
static_assert (!can_substitute (^^T, { ^^float, ^^int }));
constexpr float fv = 42.0f;
static_assert (can_substitute (^^T, { ^^fv, reflect_constant (42) }));
static_assert (can_substitute (^^T, { ^^fv, reflect_constant (0) }));
static_assert (can_substitute (^^T, { ^^fv, ^^n }));
static_assert (!can_substitute (^^T, { ^^n, ^^fv }));
static_assert (!can_substitute (^^T, { ^^fv, ^^n, ^^fv }));

static_assert (can_substitute (^^U, {}));
static_assert (can_substitute (^^U, { ^^int }));
static_assert (can_substitute (^^U, { ^^int, ^^long, ^^const int &, ^^float, ^^double }));
static_assert (!can_substitute (^^U, { ^^int, ^^long, ^^const int &, ^^n, ^^float }));

static_assert (!can_substitute (^^v, {}));
static_assert (can_substitute (^^v, { reflect_constant (15) }));
static_assert (can_substitute (^^v, { ^^n }));
static_assert (!can_substitute (^^v, { ^^n, ^^n }));
static_assert (!can_substitute (^^v, { ^^int }));

static_assert (!can_substitute (^^C, {}));
static_assert (can_substitute (^^C, { ^^int }));
static_assert (can_substitute (^^C, { ^^V }));
static_assert (!can_substitute (^^C, { ^^int, ^^int }));
static_assert (!can_substitute (^^C, { reflect_constant (42) }));
static_assert (!can_substitute (^^C, { ^^n }));

static_assert (!can_substitute (^^foo, {}));
static_assert (can_substitute (^^foo, { ^^int }));
static_assert (can_substitute (^^foo, { ^^V }));
static_assert (can_substitute (^^foo, { ^^NS }));
static_assert (!can_substitute (^^foo, { ^^int, ^^long }));
static_assert (!can_substitute (^^foo, { reflect_constant (42) }));
static_assert (!can_substitute (^^foo, { ^^n }));
