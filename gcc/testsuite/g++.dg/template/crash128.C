// PR c++/54090

template <int n>
struct X {

  template <int N, bool = (n >= N), typename T = void> struct Y;

  template <int N, typename T>
  struct Y<N, true, T> {};

  static const int M = n / 2;

  template <typename T>
  struct Y<X::M, true, T> {};
};

void foo() {
  X<10>::Y<10/2> y;
}
