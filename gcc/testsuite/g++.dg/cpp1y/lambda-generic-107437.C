// PR c++/107437
// { dg-do compile { target c++14 } }

struct integral_constant {
  constexpr operator int() const { return 42; }
};

template<int N>
struct A {
  static constexpr int value = N;
};

template<class T>
void f(T t) {
  [=](auto) {
    A<t> a; // { dg-bogus "constant" }
    return a.value;
  }(0);
}

template void f(integral_constant);
