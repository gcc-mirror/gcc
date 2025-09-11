// PR c++/121705
// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module-graph" }
// { dg-module-cmi M }

module;
namespace helpers {
  template <typename T> bool operator<(T, int);
}
namespace ns {
  struct E {};
  bool operator==(E, int);
  template <typename T> bool foo(E, T);

  // won't be found
  using helpers::operator<;  // NB [module.global.frag] p3.6
  void unused(E);
}
export module M;

export template <typename T> bool test_op(T t, int x) {
  // ensure it's not discarded
  ns::E{} == x;
  foo(ns::E{}, x);
  // { dg-final { scan-lang-dump {Built ADL binding for function_decl:'::ns::operator=='} module } }
  // { dg-final { scan-lang-dump {Built ADL binding for template_decl:'::ns::template foo'} module } }
  return t == x && foo(t, x);
}

export template <typename T> bool test_using(T t, int x) {
  // ensure it's not discarded
  ns::E{} < 0;
  // { dg-final { scan-lang-dump {Built ADL binding for template_decl:'::helpers::template operator<'} module } }
  return t < x;
}

export template <typename T> void test_unused(T t) {
  // we never use this non-dependently, so it gets discarded
  unused(t);
  // { dg-final { scan-lang-dump-not {'::ns::unused'} module } }
}

export using ns::E;
