// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::substitute.

#include <meta>
#include <ranges>
#include <vector>

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
  try { substitute (r, args); }
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
static_assert (!could_substitute (^^cls_tmpl, {}));
static_assert (!could_substitute (^^cls_tmpl<int>, {}));
static_assert (!could_substitute (^^incomplete_cls<int>, {}));
static_assert (!could_substitute (^^fun_tmpl, {}));
static_assert (!could_substitute (^^fun_tmpl<int>, {}));
static_assert (!could_substitute (^^conc, {}));
static_assert (!could_substitute (substitute (^^conc, { ^^int }), {}));
static_assert (!could_substitute (^^var_tmpl, {}));
static_assert (!could_substitute (^^var_tmpl<int>, {}));
static_assert (!could_substitute (^^cls_tmpl_alias, {}));
static_assert (!could_substitute (^^cls_tmpl_alias<int>, {}));
static_assert (!could_substitute (^^Enum, {}));
static_assert (!could_substitute (^^Enum::A, {}));
static_assert (!could_substitute (^^Enum_class, {}));
static_assert (!could_substitute (^^Enum_class::A, {}));
static_assert (!could_substitute (^^decomp, {}));
static_assert (!could_substitute (^^decomp_ref, {}));
static_assert (!could_substitute (^^arr, {}));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!could_substitute (dms, {}));

struct Base {};
struct Derived : Base {};
static_assert (!could_substitute (bases_of (^^Derived, access_context::current ())[0], {}));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!could_substitute (^^T, {}));
  static_assert (!could_substitute (R, {}));
  static_assert (!could_substitute (R2, {}));
  static_assert (!could_substitute (R3, {}));
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

static_assert (!could_substitute (^^S, {}));
static_assert (!could_substitute (^^S, { null_reflection }));
static_assert (!could_substitute (^^S, { parameters_of (^^bar)[0] }));
static_assert (!could_substitute (^^S, { annotations_of (^^bar)[0] }));
static_assert (!could_substitute (^^S, { ^^:: }));
static_assert (!could_substitute (^^S, { ^^ns }));
static_assert (!could_substitute (^^S, { ^^ns_alias }));
static_assert (!could_substitute (^^S, { ^^NS::~NS }));
static_assert (!could_substitute (^^S, { reflect_constant (42) }));
constexpr int n = 42;
static_assert (!could_substitute (^^S, { ^^n }));
constexpr NS nsv (42);
static_assert (!could_substitute (^^S, { ^^nsv }));

static_assert (!could_substitute (^^S, {}));
static_assert (substitute (^^S, { ^^int }) == ^^S <int>);
static_assert (substitute (^^S, { ^^V }) == ^^S <V>);
static_assert (substitute (^^S, { ^^NS }) == ^^S <NS>);
static_assert (!is_value (substitute (^^S, { ^^int })));
static_assert (!is_object (substitute (^^S, { ^^int })));
static_assert (!could_substitute (^^S, { ^^int, ^^long }));
static_assert (!could_substitute (^^S, { reflect_constant (42) }));
static_assert (!could_substitute (^^S, { ^^n }));
static_assert (!could_substitute (^^T, {}));
static_assert (!could_substitute (^^T, { ^^float, ^^int }));
constexpr float fv = 42.0f;
static_assert (substitute (^^T, { ^^fv, reflect_constant (42) }) == ^^T <42.0f, 42>);
static_assert (!is_value (substitute (^^T, { ^^fv, reflect_constant (42) })));
static_assert (!is_object (substitute (^^T, { ^^fv, reflect_constant (42) })));
static_assert (substitute (^^T, { ^^fv, reflect_constant (0) }) == ^^T <42.0f, 0>);
static_assert (!is_value (substitute (^^T, { ^^fv, reflect_constant (0) })));
static_assert (!is_object (substitute (^^T, { ^^fv, reflect_constant (0) })));
static_assert (substitute (^^T, { ^^fv, ^^n }) == ^^T <42.0f, 42>);
static_assert (!is_value (substitute (^^T, { ^^fv, ^^n })));
static_assert (!is_object (substitute (^^T, { ^^fv, ^^n })));
static_assert (!could_substitute (^^T, { ^^n, ^^fv }));
static_assert (!could_substitute (^^T, { ^^fv, ^^n, ^^fv }));

static_assert (substitute (^^U, {}) == ^^U <>);
static_assert (!is_value (substitute (^^U, {})));
static_assert (!is_object (substitute (^^U, {})));
static_assert (substitute (^^U, { ^^int }) == ^^U <int>);
static_assert (substitute (^^U, { ^^int, ^^long, ^^const int &, ^^float, ^^double }) == ^^U <int, long, const int &, float, double>);
static_assert (substitute (^^U, std::vector <info> { ^^int, ^^long, ^^const int &, ^^float, ^^double } | std::views::reverse) == ^^U <double, float, const int &, long, int>);
static_assert (!could_substitute (^^U, { ^^int, ^^long, ^^const int &, ^^n, ^^float }));

static_assert (!could_substitute (^^v, {}));
static_assert (substitute (^^v, { reflect_constant (15) }) == ^^v <15>);
static_assert (!is_value (substitute (^^v, { reflect_constant (15) })));
static_assert (!is_object (substitute (^^v, { reflect_constant (15) })));
static_assert (substitute (^^v, { ^^n }) == ^^v <42>);
static_assert (!is_value (substitute (^^v, { ^^n })));
static_assert (!is_object (substitute (^^v, { ^^n })));
static_assert (!could_substitute (^^v, { ^^n, ^^n }));
static_assert (!could_substitute (^^v, { ^^int }));

static_assert (!could_substitute (^^C, {}));
static_assert (substitute (^^C, { ^^int }) == reflect_constant (false));
static_assert (is_value (substitute (^^C, { ^^int })));
static_assert (!is_object (substitute (^^C, { ^^int })));
static_assert (substitute (^^C, { ^^V }) == reflect_constant (true));
static_assert (is_value (substitute (^^C, { ^^V })));
static_assert (!is_object (substitute (^^C, { ^^V })));
static_assert (!could_substitute (^^C, { ^^int, ^^int }));
static_assert (!could_substitute (^^C, { reflect_constant (42) }));
static_assert (!could_substitute (^^C, { ^^n }));

static_assert (!could_substitute (^^foo, {}));
static_assert (substitute (^^foo, { ^^int }) == ^^foo <int>);
static_assert (!is_value (substitute (^^foo, { ^^int })));
static_assert (!is_object (substitute (^^foo, { ^^int })));
static_assert (substitute (^^foo, { ^^int }) == ^^foo <int>);
static_assert (substitute (^^foo, { ^^V }) == ^^foo <V>);
static_assert (substitute (^^foo, { ^^NS }) == ^^foo <NS>);
static_assert (!could_substitute (^^foo, { ^^int, ^^long }));
static_assert (!could_substitute (^^foo, { reflect_constant (42) }));
static_assert (!could_substitute (^^foo, { ^^n }));

template <class T, class U>
struct TU1 {};
template <class T, class U>
struct TU2 {};

template <template <class, class> class T, class U, class V>
T <U, V> baz () { return T <U, V> {}; }

static_assert (return_type_of (^^baz <TU1, int, long>) == ^^TU1 <int, long>);
static_assert (return_type_of (^^baz <TU2, double, int>) == ^^TU2 <double, int>);
static_assert (substitute (^^baz, { ^^TU1, ^^int, ^^long }) == ^^baz <TU1, int, long>);
static_assert (!is_value (substitute (^^baz, { ^^TU1, ^^int, ^^long })));
static_assert (!is_object (substitute (^^baz, { ^^TU1, ^^int, ^^long })));
static_assert (substitute (^^baz, { ^^TU2, ^^double, ^^int }) == ^^baz <TU2, double, int>);
