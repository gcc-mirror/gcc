// PR c++/109651
// { dg-do compile { target c++20 } }

template<class T>
auto f() {
  return []<class U, template<class V, U, V> class TT>(U, TT<int, 1, 2>) { };
}

template<class T, int N, int M> struct A { };

int main() {
  f<int>()(0, A<int, 1, 2>{});
}
