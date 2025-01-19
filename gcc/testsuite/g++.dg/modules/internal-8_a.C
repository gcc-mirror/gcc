// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local" }
// Non-ODR usages of const variables currently are erroneously
// reported in templates and constexpr functions; this test
// XFAILS them until we can implement a fix.

export module M;

namespace { struct internal_t {}; };
static const int value = 123;
static const int& ref = 456;
static const internal_t internal {};

constexpr void f(int) {}

export constexpr
void no_odr_use_cexpr() {  // { dg-bogus "TU-local" "" { xfail *-*-* } }
  int x = value;
  int y = ref;
  int z = (internal, 0);

  value;
  bool b = value < value;
  f(value);
}

export template <typename T>
void no_odr_use_templ() {  // { dg-bogus "TU-local" "" { xfail *-*-* } }
  int x = value;
  int y = ref;
  int z = (internal, 0);

  value;
  bool b = value < value;
  f(value);
}
