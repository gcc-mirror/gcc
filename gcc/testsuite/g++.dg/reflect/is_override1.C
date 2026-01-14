// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_override.

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

static_assert (!is_override (null_reflection));
static_assert (!is_override (^^::));
static_assert (!is_override (^^ns));
static_assert (!is_override (^^ns_alias));
static_assert (!is_override (reflect_constant (3)));
static_assert (!is_override (^^cls));
static_assert (!is_override (^^cls::dm));
static_assert (!is_override (^^cls::ref_dm));
static_assert (!is_override (^^cls::static_dm));
static_assert (!is_override (^^cls::mem_fun));
static_assert (!is_override (^^cls::static_mem_fun));
static_assert (!is_override (^^cls::type));
static_assert (!is_override (^^cls_var));
static_assert (!is_override (^^onion));
static_assert (!is_override (^^anon));
static_assert (!is_override (^^fun));
static_assert (!is_override (^^alias));
static_assert (!is_override (^^var));
static_assert (!is_override (^^ref));
static_assert (!is_override (^^rref));
static_assert (!is_override (^^ptr));
static_assert (!is_override (^^cls_tmpl));
static_assert (!is_override (^^cls_tmpl<int>));
static_assert (!is_override (^^incomplete_cls<int>));
static_assert (!is_override (^^fun_tmpl));
static_assert (!is_override (^^fun_tmpl<int>));
static_assert (!is_override (^^conc));
static_assert (!is_override (substitute (^^conc, { ^^int })));
static_assert (!is_override (^^var_tmpl));
static_assert (!is_override (^^var_tmpl<int>));
static_assert (!is_override (^^cls_tmpl_alias));
static_assert (!is_override (^^cls_tmpl_alias<int>));
static_assert (!is_override (^^Enum));
static_assert (!is_override (^^Enum::A));
static_assert (!is_override (^^Enum_class));
static_assert (!is_override (^^Enum_class::A));
static_assert (!is_override (^^decomp));
static_assert (!is_override (^^decomp_ref));
static_assert (!is_override (^^arr));

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!is_override (dms));

struct Base {};
struct Derived : Base {};
static_assert (!is_override (bases_of (^^Derived, access_context::current ())[0]));

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!is_override (^^T));
  static_assert (!is_override (R));
  static_assert (!is_override (R2));
  static_assert (!is_override (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^conc>();
  static_assert (!is_override (^^p));
  static_assert (!is_override (^^c));
}

struct Base2 {
  void foo ();
  virtual void bar ();
  virtual void baz ();
  virtual void qux ();
  virtual void corge () = 0;
  virtual void garply ();
  virtual ~Base2 ();
};

struct Derived2 : Base2 {
  void foo ();
  virtual void baz ();
  virtual void qux () override;
  virtual void corge ();
  virtual void freddy ();
};

struct Derived3 : Derived2 {
  void foo ();
  virtual void qux () override;
  virtual void corge ();
  virtual void freddy ();
};

static_assert (!is_override (^^Base2::foo));
static_assert (!is_override (^^Base2::bar));
static_assert (!is_override (^^Base2::baz));
static_assert (!is_override (^^Base2::qux));
static_assert (!is_override (^^Base2::corge));
static_assert (!is_override (^^Base2::garply));
static_assert (!is_override (^^Base2::~Base2));
static_assert (!is_override (^^Derived2::foo));
static_assert (!is_override (^^Derived2::bar));
static_assert (is_override (^^Derived2::baz));
static_assert (is_override (^^Derived2::qux));
static_assert (is_override (^^Derived2::corge));
static_assert (!is_override (^^Derived2::garply));
static_assert (!is_override (^^Derived2::freddy));
static_assert (is_override (^^Derived2::~Derived2));
static_assert (!is_override (^^Derived3::foo));
static_assert (!is_override (^^Derived3::bar));
static_assert (is_override (^^Derived3::baz));
static_assert (is_override (^^Derived3::qux));
static_assert (is_override (^^Derived3::corge));
static_assert (!is_override (^^Derived3::garply));
static_assert (is_override (^^Derived3::freddy));
static_assert (is_override (^^Derived3::~Derived3));
