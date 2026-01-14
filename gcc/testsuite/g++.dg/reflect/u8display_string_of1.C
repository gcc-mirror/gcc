// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::u8display_string_of.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
void fn();
auto &fn2();
enum Enum { A };
using Alias = int;
struct B {};
struct S : B {
  int mem;
  int : 0;
};
struct T {
  T () {}
  T (const T &) {}
  ~T () {}
};
struct U {
  int u;
  int v : 5;
};
template<auto> struct TCls {};
template<auto> void TFn();
template<auto> int TVar;
template<auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;
namespace {
  namespace NS2 {}
};
namespace NS3 {
  namespace {
    namespace {
      namespace NS4 {}
    }
  }
};
enum { Z };

constexpr auto ctx = access_context::current ();

struct AN { int a; long b; char c; };

[[=1, =AN { 1, 42, ' ' }]] void bar (long, const T f, int g[2], T &);

struct V1 {
  constexpr V1 (int) {}
};
struct V2 {
  V2 &operator = (const V2 &);
};
struct V3 {
  V3 &operator = (V3 &&);
};
struct V4 {
  V4 &operator += (const V4 &);
};
struct V5 {
  operator int ();
};
int operator ""_a (const char *);

void
foo (int a, const long b, T c, int d[4], T &e)
{
  static_assert (u8display_string_of (^^a) == u8"a");
  static_assert (u8display_string_of (^^b) == u8"b");
  static_assert (u8display_string_of (^^c) == u8"c");
  static_assert (u8display_string_of (^^d) == u8"d");
  static_assert (u8display_string_of (^^e) == u8"e");
  static_assert (u8display_string_of (parameters_of (^^foo)[0]) == u8"<parameter a of void foo(int, long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^foo)[1]) == u8"<parameter b of void foo(int, long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^foo)[2]) == u8"<parameter c of void foo(int, long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^foo)[3]) == u8"<parameter d of void foo(int, long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^foo)[4]) == u8"<parameter e of void foo(int, long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^bar)[0]) == u8"<unnamed parameter 1 of void bar(long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^bar)[1]) == u8"<parameter f of void bar(long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^bar)[2]) == u8"<parameter g of void bar(long int, T, int*, T&)>");
  static_assert (u8display_string_of (parameters_of (^^bar)[3]) == u8"<unnamed parameter 4 of void bar(long int, T, int*, T&)>");
  static_assert (u8display_string_of (null_reflection) == u8"<null reflection>");
  static_assert (u8display_string_of (^^::) == u8"::");
  static_assert (u8display_string_of (^^NS2) == u8"{anonymous}::NS2");
  static_assert (u8display_string_of (parent_of (^^NS2)) == u8"{anonymous}");
  static_assert (u8display_string_of (^^NS3::NS4) == u8"NS3::{anonymous}::{anonymous}::NS4");
  static_assert (u8display_string_of (parent_of (^^NS3::NS4)) == u8"NS3::{anonymous}::{anonymous}");
  static_assert (u8display_string_of (parent_of (parent_of (^^NS3::NS4))) == u8"NS3::{anonymous}");
  static_assert (u8display_string_of (parent_of (parent_of (parent_of   (^^NS3::NS4)))) == u8"NS3");
  static_assert (u8display_string_of (^^Z) == u8"Z");
  static_assert (u8display_string_of (parent_of (^^Z)) == u8"<unnamed enum>");
  static_assert (u8display_string_of (reflect_constant (42)) == u8"42");
  static_assert (u8display_string_of (reflect_object (arr[1])) == u8"arr[1]");
  static_assert (u8display_string_of (^^arr) == u8"arr");
  static_assert (u8display_string_of (^^a3) == u8"a3");
  static_assert (u8display_string_of (^^fn) == u8"void fn()");
  static_assert (u8display_string_of (^^fn2) == u8"auto& fn2()");
  static_assert (u8display_string_of (^^Enum::A) == u8"A");
  static_assert (u8display_string_of (^^Alias) == u8"Alias {aka int}");
  static_assert (u8display_string_of (^^S) == u8"S");
  static_assert (u8display_string_of (^^S::mem) == u8"S::mem");
  static_assert (u8display_string_of (members_of (^^S, ctx)[1]) == u8"S::<unnamed bit-field>");
  static_assert (u8display_string_of (^^TCls) == u8"template<auto <anonymous> > struct TCls");
  static_assert (u8display_string_of (^^TFn) == u8"template<auto <anonymous> > void TFn()");
  static_assert (u8display_string_of (^^TVar) == u8"template<auto <anonymous> > int TVar<<anonymous> >");
  static_assert (u8display_string_of (^^Concept) == u8"template<auto <anonymous> > concept Concept");
  static_assert (u8display_string_of (^^NSAlias) == u8"NSAlias");
  static_assert (u8display_string_of (^^NS) == u8"NS");
  static_assert (u8display_string_of (bases_of (^^S, ctx)[0]) == u8"S: B");
  static_assert (u8display_string_of (data_member_spec (^^int, { .name = "member", .alignment = 128, .no_unique_address = true })) == u8"(int, member, 128, , true)");
  static_assert (u8display_string_of (data_member_spec (^^const int, { .name = "member", .bit_width = 6 })) == u8"(const int, member, , 6, false)");
  static_assert (u8display_string_of (data_member_spec (^^int, { .bit_width = 0 })) == u8"(int, , , 0, false)");
  static_assert (u8display_string_of (data_member_spec (^^long, { .bit_width = 5 })) == u8"(long int, , , 5, false)");
  static_assert (u8display_string_of (annotations_of (^^bar)[0]) == u8"[[=1]]");
  static_assert (u8display_string_of (annotations_of (^^bar)[1]) == u8"[[=AN{1, 42, ' '}]]");
  static_assert (u8display_string_of (^^int) == u8"int");
  static_assert (u8display_string_of (^^unsigned) == u8"unsigned int");
  static_assert (u8display_string_of (^^unsigned long) == u8"long unsigned int");
  static_assert (u8display_string_of (^^const long long &) == u8"const long long int&");
  static_assert (u8display_string_of (^^const double **) == u8"const double**");
  static_assert (u8display_string_of (^^int (int)) == u8"int(int)");
  static_assert (u8display_string_of (^^int (&) (int, long, S &)) == u8"int (&)(int, long int, S&)");
  static_assert (u8display_string_of (^^int (*) (int, long, S &)) == u8"int (*)(int, long int, S&)");
  static_assert (u8display_string_of (members_of (^^V2, ctx)[0]) == u8"V2& V2::operator=(const V2&)");
  static_assert (u8display_string_of (members_of (^^V3, ctx)[0]) == u8"V3& V3::operator=(V3&&)");
  static_assert (u8display_string_of (members_of (^^V4, ctx)[0]) == u8"V4& V4::operator+=(const V4&)");
  static_assert (u8display_string_of (members_of (^^V5, ctx)[0]) == u8"V5::operator int()");
  static_assert (u8display_string_of (^^operator""_a) == u8"int operator\"\"_a(const char*)");
}

namespace NS5 {
  int arr[] = {1, 2, 3};
  auto [a1, a2, a3] = arr;
  void fn();
  auto &fn2();
  enum Enum { A };
  using Alias = int;
  struct B {};
  struct S : B {
    int mem;
    int : 0;
  };
  struct T {
    T () {}
    T (const T &) {}
    ~T () {}
  };
  struct U {
    int u;
    int v : 5;
  };
  template<auto> struct TCls {};
  template<auto> void TFn();
  template<auto> int TVar;
  template<auto> concept Concept = requires { true; };
  enum { Z };

  struct AN { int a; long b; char c; };

  [[=1, =AN { 1, 42, ' ' }]] void bar (long, const T f, int g[2], T &);

  struct V1 {
    constexpr V1 (int) {}
  };
  struct V2 {
    V2 &operator = (const V2 &);
  };
  struct V3 {
    V3 &operator = (V3 &&);
  };
  struct V4 {
    V4 &operator += (const V4 &);
  };
  struct V5 {
    operator int ();
  };
  int operator ""_a (const char *);

  void
  foo (int a, const long b, T c, int d[4], T &e)
  {
    static_assert (u8display_string_of (^^a) == u8"a");
    static_assert (u8display_string_of (^^b) == u8"b");
    static_assert (u8display_string_of (^^c) == u8"c");
    static_assert (u8display_string_of (^^d) == u8"d");
    static_assert (u8display_string_of (^^e) == u8"e");
    static_assert (u8display_string_of (parameters_of (^^foo)[0]) == u8"<parameter a of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^foo)[1]) == u8"<parameter b of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^foo)[2]) == u8"<parameter c of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^foo)[3]) == u8"<parameter d of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^foo)[4]) == u8"<parameter e of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^bar)[0]) == u8"<unnamed parameter 1 of void NS5::bar(long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^bar)[1]) == u8"<parameter f of void NS5::bar(long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^bar)[2]) == u8"<parameter g of void NS5::bar(long int, T, int*, T&)>");
    static_assert (u8display_string_of (parameters_of (^^bar)[3]) == u8"<unnamed parameter 4 of void NS5::bar(long int, T, int*, T&)>");
    static_assert (u8display_string_of (^^Z) == u8"NS5::Z");
    static_assert (u8display_string_of (parent_of (^^Z)) == u8"NS5::<unnamed enum>");
    static_assert (u8display_string_of (reflect_constant (42)) == u8"42");
    static_assert (u8display_string_of (reflect_object (arr[1])) == u8"NS5::arr[1]");
    static_assert (u8display_string_of (^^arr) == u8"NS5::arr");
    static_assert (u8display_string_of (^^a3) == u8"NS5::a3");
    static_assert (u8display_string_of (^^fn) == u8"void NS5::fn()");
    static_assert (u8display_string_of (^^fn2) == u8"auto& NS5::fn2()");
    static_assert (u8display_string_of (^^Enum::A) == u8"NS5::A");
    static_assert (u8display_string_of (^^Alias) == u8"NS5::Alias {aka int}");
    static_assert (u8display_string_of (^^S) == u8"NS5::S");
    static_assert (u8display_string_of (^^S::mem) == u8"NS5::S::mem");
    static_assert (u8display_string_of (members_of (^^S, ctx)[1]) == u8"NS5::S::<unnamed bit-field>");
    static_assert (u8display_string_of (^^TCls) == u8"template<auto <anonymous> > struct NS5::TCls");
    static_assert (u8display_string_of (^^TFn) == u8"template<auto <anonymous> > void NS5::TFn()");
    static_assert (u8display_string_of (^^TVar) == u8"template<auto <anonymous> > int NS5::TVar<<anonymous> >");
    static_assert (u8display_string_of (^^Concept) == u8"template<auto <anonymous> > concept NS5::Concept");
    static_assert (u8display_string_of (bases_of (^^S, ctx)[0]) == u8"NS5::S: NS5::B");
    static_assert (u8display_string_of (annotations_of (^^bar)[0]) == u8"[[=1]]");
    static_assert (u8display_string_of (annotations_of (^^bar)[1]) == u8"[[=NS5::AN{1, 42, ' '}]]");
    static_assert (u8display_string_of (^^int (&) (int, long, S &)) == u8"int (&)(int, long int, NS5::S&)");
    static_assert (u8display_string_of (^^int (*) (int, long, S &)) == u8"int (*)(int, long int, NS5::S&)");
    static_assert (u8display_string_of (members_of (^^V2, ctx)[0]) == u8"NS5::V2& NS5::V2::operator=(const NS5::V2&)");
    static_assert (u8display_string_of (members_of (^^V3, ctx)[0]) == u8"NS5::V3& NS5::V3::operator=(NS5::V3&&)");
    static_assert (u8display_string_of (members_of (^^V4, ctx)[0]) == u8"NS5::V4& NS5::V4::operator+=(const NS5::V4&)");
    static_assert (u8display_string_of (members_of (^^V5, ctx)[0]) == u8"NS5::V5::operator int()");
    static_assert (u8display_string_of (^^operator""_a) == u8"int NS5::operator\"\"_a(const char*)");
  }
}
