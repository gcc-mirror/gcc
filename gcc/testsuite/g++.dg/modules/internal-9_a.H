// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

static int x = 123;
static void f() {}
template <typename T> static void t() {}

namespace {
  int y = 456;
  void g() {};
  template <typename T> void u() {}

  namespace ns { int in_ns = 456; }

  struct A {};
  template <typename T> struct B {};

  enum E { X };
  enum class F { Y };

  template <typename T> using U = int;

#if __cplusplus >= 202002L
  template <typename T> concept C = true;
#endif
}

namespace ns2 = ns;
