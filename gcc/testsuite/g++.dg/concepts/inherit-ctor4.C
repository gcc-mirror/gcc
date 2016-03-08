// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  struct S1 {
    template<C U> S1(U x) { }
  };

template<typename T>
  struct S2 : S1<T> {
    using S1<T>::S1;
  };

int main() {
  S2<int> s(0); // { dg-error "no matching function" }
}
