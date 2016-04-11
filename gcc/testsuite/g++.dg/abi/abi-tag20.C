// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1B1gIcEEN7__cxx111XEv" } }

inline namespace __cxx11 __attribute__((__abi_tag__ ("ABI_TAG"))) {
  class X {};
}
struct B {
  X f();
  template <class U> X g();
};
int main() {
  B b;
  b.g<char>();
  return 0;
}
