// PR c++/56135
// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for functional function" { ! hostedlib } }

#include <functional>

struct test {
  template<typename T>
  std::function<void()> broken(int x) {
    return [=] { +x; print<T>(); }; // { dg-warning "implicit capture" "" { target c++2a } }
  }

  std::function<void()> works0() {
    return [=] { print<int>(); }; // { dg-warning "implicit capture" "" { target c++2a } }
  }

  template<typename T>
  std::function<void()> works1() {
    return [=] { print<int>(); }; // { dg-warning "implicit capture" "" { target c++2a } }
  }

  template<typename T>
  std::function<void()> works2() {
    return [=] { this->print<T>(); }; // { dg-warning "implicit capture" "" { target c++2a } }
  }

  template<typename T>
  void print() { if (this == 0) __builtin_abort (); }
};

int main(void) {
  test().broken<int>(1)();
  test().works0()();
  test().works1<int>()();
  test().works2<int>()();

  return 0;
}
