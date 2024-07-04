// PR c++/114867
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

namespace ns {
  template <typename T> void f(T);

  namespace inner {
    class E {};
    int f(E);
  }
  using inner::f;
}

export module M;

template <typename T>
struct X {
  void test() { ns::f(T{}); }
};
template struct X<int>;

export namespace ns {
  using ns::f;
}

export auto get_e() {
  return ns::inner::E{};
}
