// Verify that deduction failure of the decltype(auto) template parameter is
// a SFINAE error.
// { dg-do compile { target c++17 } }

template <class> void f();
template <class> void f(int);

template <class T = int, decltype(auto) = &f<T>> void g();
template <class = int> void g();

int main() {
  g();
}
