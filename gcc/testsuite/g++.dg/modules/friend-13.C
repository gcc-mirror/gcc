// PR c++/124390
// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;
template <typename T> struct tuple {
  template <typename U> friend void f(tuple, U);
};
template <typename T> tuple<T> make_unique();
export module M;
void test() {
  auto lambda = [] {};
  make_unique<decltype(lambda)>();
}

// Hidden friends should be discarded if not used.
// { dg-final { scan-lang-dump-not {Bindings '::f'} module } }
