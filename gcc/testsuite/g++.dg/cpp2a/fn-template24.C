// PR c++/99911
// { dg-do compile { target c++20 } }

namespace N {
  struct A { };
  template<class T> void get(A);
};

template<class T>
auto f() {
  return []<class U>(U) { get<U>(T{}); };
}

int main() {
  f<N::A>()(0);
}
