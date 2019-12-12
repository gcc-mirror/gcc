// { dg-do compile { target c++2a } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  struct S1 {
    template<C U> S1(U x) { }
  };

template<typename T>
  struct S2 : S1<T> {
    using S1<T>::S1; // { dg-error "no matching function" }
  };

int main() {
  S2<int> s(0); // { dg-error "use of deleted function" }
}
