// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  struct S1 {
    template<C U>
      S1(U x) { }
  };

template<typename T>
  struct S2 : S1<T> {
    using S1<T>::S1;
  };

struct X { } x;

int main() {
  S2<X> s = x;
}
