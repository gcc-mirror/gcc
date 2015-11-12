void f();

namespace A {
  template <class T> void f(T) { }
  using ::f;
  template void f(int);
}
