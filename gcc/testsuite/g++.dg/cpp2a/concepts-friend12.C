// PR c++/107853
// { dg-do compile { target c++20 } }

template<class T, class U>
concept C = __is_same(T, U);

template<class... Ts>
struct A {
  template<class... Us>
    requires (C<Ts, Us> && ...)
  friend void f(A, A<Us...>) { }
};

int main() {
  A<int> x;
  f(x, x);
  A<int, int> y;
  f(y, y);
  A<char> z;
  f(x, z); // { dg-error "no match" }
}
