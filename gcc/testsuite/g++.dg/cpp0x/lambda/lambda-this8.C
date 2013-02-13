// PR c++/56135
// { dg-do run { target c++11 } }

#include <functional>

extern "C" void abort() throw();

struct test {
  template<typename T>
  std::function<void()> broken(int x) {
    return [=] { +x; print<T>(); };
  }

  std::function<void()> works0() {
    return [=] { print<int>(); };
  }

  template<typename T>
  std::function<void()> works1() {
    return [=] { print<int>(); };
  }

  template<typename T>
  std::function<void()> works2() {
    return [=] { this->print<T>(); };
  }

  template<typename T>
  void print() { if (this == NULL) abort (); }
};

int main(void) {
  test().broken<int>(1)();
  test().works0()();
  test().works1<int>()();
  test().works2<int>()();

  return 0;
}
