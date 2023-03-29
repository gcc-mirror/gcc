// PR c++/105518
// { dg-do compile { target c++11 } }

struct integral_constant {
  constexpr operator int() const { return 42; }
};

template<int N>
struct A {
  using type = A;
  static constexpr int value = N;
};

template<class T>
void f(T t) {
  using alias = A<t>;
  [](int) {
    typename alias::type a; // { dg-bogus "'t' is not captured" }
    return a.value;
  }(0);
}

template void f(integral_constant);
