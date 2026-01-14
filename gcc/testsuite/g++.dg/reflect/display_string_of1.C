// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::display_string_of.

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
  static_assert (display_string_of (^^a) == "a");
  static_assert (display_string_of (^^b) == "b");
  static_assert (display_string_of (^^c) == "c");
  static_assert (display_string_of (^^d) == "d");
  static_assert (display_string_of (^^e) == "e");
  static_assert (display_string_of (parameters_of (^^foo)[0]) == "<parameter a of void foo(int, long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^foo)[1]) == "<parameter b of void foo(int, long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^foo)[2]) == "<parameter c of void foo(int, long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^foo)[3]) == "<parameter d of void foo(int, long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^foo)[4]) == "<parameter e of void foo(int, long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^bar)[0]) == "<unnamed parameter 1 of void bar(long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^bar)[1]) == "<parameter f of void bar(long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^bar)[2]) == "<parameter g of void bar(long int, T, int*, T&)>");
  static_assert (display_string_of (parameters_of (^^bar)[3]) == "<unnamed parameter 4 of void bar(long int, T, int*, T&)>");
  static_assert (display_string_of (null_reflection) == "<null reflection>");
  static_assert (display_string_of (^^::) == "::");
  static_assert (display_string_of (^^NS2) == "{anonymous}::NS2");
  static_assert (display_string_of (parent_of (^^NS2)) == "{anonymous}");
  static_assert (display_string_of (^^NS3::NS4) == "NS3::{anonymous}::{anonymous}::NS4");
  static_assert (display_string_of (parent_of (^^NS3::NS4)) == "NS3::{anonymous}::{anonymous}");
  static_assert (display_string_of (parent_of (parent_of (^^NS3::NS4))) == "NS3::{anonymous}");
  static_assert (display_string_of (parent_of (parent_of (parent_of   (^^NS3::NS4)))) == "NS3");
  static_assert (display_string_of (^^Z) == "Z");
  static_assert (display_string_of (parent_of (^^Z)) == "<unnamed enum>");
  static_assert (display_string_of (reflect_constant (42)) == "42");
  static_assert (display_string_of (reflect_object (arr[1])) == "arr[1]");
  static_assert (display_string_of (^^arr) == "arr");
  static_assert (display_string_of (^^a3) == "a3");
  static_assert (display_string_of (^^fn) == "void fn()");
  static_assert (display_string_of (^^fn2) == "auto& fn2()");
  static_assert (display_string_of (^^Enum::A) == "A");
  static_assert (display_string_of (^^Alias) == "Alias {aka int}");
  static_assert (display_string_of (^^S) == "S");
  static_assert (display_string_of (^^S::mem) == "S::mem");
  static_assert (display_string_of (members_of (^^S, ctx)[1]) == "S::<unnamed bit-field>");
  static_assert (display_string_of (^^TCls) == "template<auto <anonymous> > struct TCls");
  static_assert (display_string_of (^^TFn) == "template<auto <anonymous> > void TFn()");
  static_assert (display_string_of (^^TVar) == "template<auto <anonymous> > int TVar<<anonymous> >");
  static_assert (display_string_of (^^Concept) == "template<auto <anonymous> > concept Concept");
  static_assert (display_string_of (^^NSAlias) == "NSAlias");
  static_assert (display_string_of (^^NS) == "NS");
  static_assert (display_string_of (bases_of (^^S, ctx)[0]) == "S: B");
  static_assert (display_string_of (data_member_spec (^^int, { .name = "member", .alignment = 128, .no_unique_address = true })) == "(int, member, 128, , true)");
  static_assert (display_string_of (data_member_spec (^^const int, { .name = "member", .bit_width = 6 })) == "(const int, member, , 6, false)");
  static_assert (display_string_of (data_member_spec (^^int, { .bit_width = 0 })) == "(int, , , 0, false)");
  static_assert (display_string_of (data_member_spec (^^long, { .bit_width = 5 })) == "(long int, , , 5, false)");
  static_assert (display_string_of (annotations_of (^^bar)[0]) == "[[=1]]");
  static_assert (display_string_of (annotations_of (^^bar)[1]) == "[[=AN{1, 42, ' '}]]");
  static_assert (display_string_of (^^int) == "int");
  static_assert (display_string_of (^^unsigned) == "unsigned int");
  static_assert (display_string_of (^^unsigned long) == "long unsigned int");
  static_assert (display_string_of (^^const long long &) == "const long long int&");
  static_assert (display_string_of (^^const double **) == "const double**");
  static_assert (display_string_of (^^int (int)) == "int(int)");
  static_assert (display_string_of (^^int (&) (int, long, S &)) == "int (&)(int, long int, S&)");
  static_assert (display_string_of (^^int (*) (int, long, S &)) == "int (*)(int, long int, S&)");
  static_assert (display_string_of (members_of (^^V2, ctx)[0]) == "V2& V2::operator=(const V2&)");
  static_assert (display_string_of (members_of (^^V3, ctx)[0]) == "V3& V3::operator=(V3&&)");
  static_assert (display_string_of (members_of (^^V4, ctx)[0]) == "V4& V4::operator+=(const V4&)");
  static_assert (display_string_of (members_of (^^V5, ctx)[0]) == "V5::operator int()");
  static_assert (display_string_of (^^operator""_a) == "int operator\"\"_a(const char*)");
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
    static_assert (display_string_of (^^a) == "a");
    static_assert (display_string_of (^^b) == "b");
    static_assert (display_string_of (^^c) == "c");
    static_assert (display_string_of (^^d) == "d");
    static_assert (display_string_of (^^e) == "e");
    static_assert (display_string_of (parameters_of (^^foo)[0]) == "<parameter a of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^foo)[1]) == "<parameter b of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^foo)[2]) == "<parameter c of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^foo)[3]) == "<parameter d of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^foo)[4]) == "<parameter e of void NS5::foo(int, long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^bar)[0]) == "<unnamed parameter 1 of void NS5::bar(long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^bar)[1]) == "<parameter f of void NS5::bar(long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^bar)[2]) == "<parameter g of void NS5::bar(long int, T, int*, T&)>");
    static_assert (display_string_of (parameters_of (^^bar)[3]) == "<unnamed parameter 4 of void NS5::bar(long int, T, int*, T&)>");
    static_assert (display_string_of (^^Z) == "NS5::Z");
    static_assert (display_string_of (parent_of (^^Z)) == "NS5::<unnamed enum>");
    static_assert (display_string_of (reflect_constant (42)) == "42");
    static_assert (display_string_of (reflect_object (arr[1])) == "NS5::arr[1]");
    static_assert (display_string_of (^^arr) == "NS5::arr");
    static_assert (display_string_of (^^a3) == "NS5::a3");
    static_assert (display_string_of (^^fn) == "void NS5::fn()");
    static_assert (display_string_of (^^fn2) == "auto& NS5::fn2()");
    static_assert (display_string_of (^^Enum::A) == "NS5::A");
    static_assert (display_string_of (^^Alias) == "NS5::Alias {aka int}");
    static_assert (display_string_of (^^S) == "NS5::S");
    static_assert (display_string_of (^^S::mem) == "NS5::S::mem");
    static_assert (display_string_of (members_of (^^S, ctx)[1]) == "NS5::S::<unnamed bit-field>");
    static_assert (display_string_of (^^TCls) == "template<auto <anonymous> > struct NS5::TCls");
    static_assert (display_string_of (^^TFn) == "template<auto <anonymous> > void NS5::TFn()");
    static_assert (display_string_of (^^TVar) == "template<auto <anonymous> > int NS5::TVar<<anonymous> >");
    static_assert (display_string_of (^^Concept) == "template<auto <anonymous> > concept NS5::Concept");
    static_assert (display_string_of (bases_of (^^S, ctx)[0]) == "NS5::S: NS5::B");
    static_assert (display_string_of (annotations_of (^^bar)[0]) == "[[=1]]");
    static_assert (display_string_of (annotations_of (^^bar)[1]) == "[[=NS5::AN{1, 42, ' '}]]");
    static_assert (display_string_of (^^int (&) (int, long, S &)) == "int (&)(int, long int, NS5::S&)");
    static_assert (display_string_of (^^int (*) (int, long, S &)) == "int (*)(int, long int, NS5::S&)");
    static_assert (display_string_of (members_of (^^V2, ctx)[0]) == "NS5::V2& NS5::V2::operator=(const NS5::V2&)");
    static_assert (display_string_of (members_of (^^V3, ctx)[0]) == "NS5::V3& NS5::V3::operator=(NS5::V3&&)");
    static_assert (display_string_of (members_of (^^V4, ctx)[0]) == "NS5::V4& NS5::V4::operator+=(const NS5::V4&)");
    static_assert (display_string_of (members_of (^^V5, ctx)[0]) == "NS5::V5::operator int()");
    static_assert (display_string_of (^^operator""_a) == "int NS5::operator\"\"_a(const char*)");
  }
}
