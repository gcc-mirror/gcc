// C++26 expansion statements should not ICE
// { dg-additional-options "-fmodules -std=c++26" }
// { dg-module-cmi M }

export module M;

namespace ns {
  export struct S {};
  void foo(S) {}
}

export void test1() {
  template for (auto x : { ns::S{} }) {
    foo(x);
  }
}

export template <typename T>
void test2(T t) {
  template for (auto x : { t, t, t }) {
    foo(x);
  }
}
