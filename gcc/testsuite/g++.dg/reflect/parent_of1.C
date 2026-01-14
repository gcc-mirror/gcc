// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parent_of.

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

consteval bool
has_parent_of (info r)
{
  try { parent_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_parent_of (null_reflection));
static_assert (!has_parent_of (^^::));
static_assert (parent_of (^^ns) == ^^::);
static_assert (parent_of (^^std) == ^^::);
static_assert (parent_of (^^ns_alias) == ^^::);
static_assert (!has_parent_of (reflect_constant (3)));
static_assert (parent_of (^^cls) == ^^::);
static_assert (parent_of (^^cls::dm) == ^^cls);
static_assert (parent_of (^^cls::ref_dm) == ^^cls);
static_assert (parent_of (^^cls::static_dm) == ^^cls);
static_assert (parent_of (^^cls::mem_fun) == ^^cls);
static_assert (parent_of (^^cls::static_mem_fun) == ^^cls);
static_assert (parent_of (^^cls::type) == ^^cls);
static_assert (parent_of (^^cls_var) == ^^::);
static_assert (parent_of (^^onion) == ^^::);
static_assert (is_union_type (parent_of (^^anon)));
static_assert (parent_of (parent_of (^^anon)) == ^^::);
static_assert (parent_of (^^fun) == ^^::);
static_assert (parent_of (^^alias) == ^^::);
static_assert (parent_of (^^var) == ^^::);
static_assert (parent_of (^^ref) == ^^::);
static_assert (parent_of (^^rref) == ^^::);
static_assert (parent_of (^^ptr) == ^^::);
static_assert (parent_of (^^cls_tmpl) == ^^::);
static_assert (parent_of (^^cls_tmpl<int>) == ^^::);
static_assert (parent_of (^^incomplete_cls<int>) == ^^::);
static_assert (parent_of (^^fun_tmpl) == ^^::);
static_assert (parent_of (^^fun_tmpl<int>) == ^^::);
static_assert (parent_of (^^conc) == ^^::);
static_assert (!has_parent_of (substitute (^^conc, { ^^int })));
static_assert (parent_of (^^var_tmpl) == ^^::);
static_assert (parent_of (^^var_tmpl<int>) == ^^::);
static_assert (parent_of (^^cls_tmpl_alias) == ^^::);
static_assert (parent_of (^^cls_tmpl_alias<int>) == ^^::);
static_assert (parent_of (^^Enum) == ^^::);
static_assert (parent_of (^^Enum::A) == ^^Enum);
static_assert (parent_of (^^A) == ^^Enum);
static_assert (parent_of (^^Enum_class) == ^^::);
static_assert (parent_of (^^Enum_class::A) == ^^Enum_class);
static_assert (parent_of (^^decomp) == ^^::);
static_assert (parent_of (^^decomp_ref) == ^^::);
static_assert (parent_of (^^arr) == ^^::);

constexpr auto dms = data_member_spec (^^int, { .name = "dms" });
static_assert (!has_parent_of (dms));

constexpr auto ctx = std::meta::access_context::current ();

struct Base {};
struct Derived : Base {};
static_assert (parent_of (bases_of (^^Derived, ctx)[0]) == ^^Derived);

consteval {
  int a = 42;
  static_assert (parent_of (^^a) == ^^::);
  consteval {
    int b = 42;
    static_assert (parent_of (^^b) == ^^::);
  }
}

template<typename T, info R, info R2, info R3>
void
f ()
{
  static_assert (!has_parent_of (^^T));
//  static_assert (parent_of (R));
//  static_assert (parent_of (R2));
//  static_assert (parent_of (R3));
}

void
g (int p, cls c)
{
  f<int, ^^var, ^^ns, ^^cls>();
  static_assert (parent_of (^^p) == ^^g);
  static_assert (parent_of (^^c) == ^^g);
  static_assert (parent_of (parameters_of (^^g)[0]) == ^^g);
  static_assert (parent_of (^^g) == ^^::);
  consteval {
    int a = 42;
    static_assert (parent_of (^^a) == ^^g);
    consteval {
      int b = 42;
      static_assert (parent_of (^^b) == ^^g);
      consteval {
	int c = 42;
	static_assert (parent_of (^^c) == ^^g);
      }
    }
  }
}

int
main ()
{
}

namespace N
{
  extern "C" void foo (int);
  void baz (int);
  namespace M
  {
    namespace O
    {
      struct S {};
    }
  }
  consteval {
    int a = 42;
    static_assert (parent_of (^^a) == ^^N);
    consteval {
      int b = 42;
      static_assert (parent_of (^^b) == ^^N);
    }
  }
}
extern "C" void bar ();

static_assert (!has_parent_of (^^main));
static_assert (!has_parent_of (^^N::foo));
static_assert (!has_parent_of (^^bar));
static_assert (parent_of (^^N::baz) == ^^N);
static_assert (parent_of (^^N::M::O::S) == ^^N::M::O);
static_assert (parent_of (^^N::M::O) == ^^N::M);
static_assert (parent_of (^^N::M) == ^^N);
static_assert (parent_of (^^N) == ^^::);

enum F {
  F1,
  F2 = parent_of (^^F1) == ^^F,
  F3 = (parent_of (^^F2) == ^^F) + 1,
};
static_assert (F2 == 1 && F3 == 2);

void
qux ()
{
  auto a = [] ()
    {
      int b;
      static_assert (is_function (parent_of (^^b)));
      static_assert (is_class_type (parent_of (parent_of (^^b))));
      static_assert (parent_of (parent_of (parent_of (^^b))) == ^^qux);
    };
  static_assert (parent_of (^^a) == ^^qux);
  static_assert (parent_of (type_of (^^a)) == ^^qux);
}

struct I { };

struct J : I {
  union {
    int o;
  };

  enum N {
    A
  };

  consteval {
    int a = 42;
    static_assert (parent_of (^^a) == ^^J);
    consteval {
      int b = 42;
      static_assert (parent_of (^^b) == ^^J);
      consteval {
	int c = 42;
	static_assert (parent_of (^^c) == ^^J);
      }
    }
  }
};

static_assert (parent_of (^^J) == ^^::);
static_assert (parent_of (bases_of (^^J, ctx)[0]) == ^^J);
static_assert (is_union_type (parent_of (^^J::o)));
static_assert (parent_of(^^J::N) == ^^J);
static_assert (parent_of(^^J::A) == ^^J::N);

static union { union { int anon2; union { int anon3; }; }; };
static_assert (is_union_type (parent_of (^^anon2)));
static_assert (is_union_type (parent_of (parent_of (^^anon2))));
static_assert (parent_of (parent_of (parent_of (^^anon2))) == ^^::);
static_assert (is_union_type (parent_of (^^anon3)));
static_assert (is_union_type (parent_of (parent_of (^^anon3))));
static_assert (is_union_type (parent_of (parent_of (parent_of (^^anon3)))));
static_assert (parent_of (parent_of (parent_of (parent_of (^^anon3)))) == ^^::);
