// PR c++/110809
// { dg-do compile { target c++20 } }

template<class, double> struct A { };

template<class T> void f(A<T, 1.0>);
template<class T> void f(A<T, 2.0>);

int main() {
  f(A<int, 1.0>{});
  f(A<int, 1.1>{}); // { dg-error "no match" }
}
