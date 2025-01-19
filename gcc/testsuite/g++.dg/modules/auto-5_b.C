// PR c++/118049
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi B }

module;
template <typename T> struct S {
  auto foo() {}
};
template struct S<char>;
export module B;
import A;
template <typename> void x() {
  S<char>{}.foo();
}
