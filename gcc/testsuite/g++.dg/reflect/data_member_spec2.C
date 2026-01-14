// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::data_member_spec.

#include <meta>

using namespace std::meta;

consteval bool
valid_data_member_spec (info r, data_member_options opts)
{
  try { data_member_spec (r, opts); }
  catch (std::meta::exception &) { return false; }
  return true;
}

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

static_assert (!valid_data_member_spec (null_reflection, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^::, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^ns, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^ns_alias, { .name = "dms" }));
static_assert (!valid_data_member_spec (reflect_constant (3), { .name = "dms" }));
static_assert (valid_data_member_spec (^^cls, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls::dm, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls::ref_dm, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls::static_dm, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls::mem_fun, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls::static_mem_fun, { .name = "dms" }));
static_assert (valid_data_member_spec (^^cls::type, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls_var, { .name = "dms" }));
static_assert (valid_data_member_spec (^^onion, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^anon, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^fun, { .name = "dms" }));
static_assert (valid_data_member_spec (^^alias, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^var, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^ref, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^rref, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^ptr, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls_tmpl, { .name = "dms" }));
static_assert (valid_data_member_spec (^^cls_tmpl<int>, { .name = "dms" }));
static_assert (valid_data_member_spec (^^incomplete_cls<int>, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^fun_tmpl, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^fun_tmpl<int>, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^conc, { .name = "dms" }));
static_assert (!valid_data_member_spec (substitute (^^conc, { ^^int }), { .name = "dms" }));
static_assert (!valid_data_member_spec (^^var_tmpl, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^var_tmpl<int>, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^cls_tmpl_alias, { .name = "dms" }));
static_assert (valid_data_member_spec (^^cls_tmpl_alias<int>, { .name = "dms" }));
static_assert (valid_data_member_spec (^^Enum, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^Enum::A, { .name = "dms" }));
static_assert (valid_data_member_spec (^^Enum_class, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^Enum_class::A, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^decomp, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^decomp_ref, { .name = "dms" }));
static_assert (!valid_data_member_spec (^^arr, { .name = "dms" }));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!valid_data_member_spec (dms, { .name = "dms" }));

struct Base {};
struct Derived : Base {};
static_assert (!valid_data_member_spec (bases_of (^^Derived, access_context::unchecked ())[0], { .name = "dms" }));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (valid_data_member_spec (^^T, { .name = "dms" }));
  static_assert (!valid_data_member_spec (R, { .name = "dms" }));
  static_assert (!valid_data_member_spec (R2, { .name = "dms" }));
  static_assert (valid_data_member_spec (R3, { .name = "dms" }));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (!valid_data_member_spec (^^p, { .name = "dms" }));
  static_assert (!valid_data_member_spec (^^c, { .name = "dms" }));
}
