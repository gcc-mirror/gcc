// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  struct S1 {
    S1(double) requires C<T>() { }
  };

template<typename T>
  struct S2 : S1<T> { // { dg-error "matching" }
    using S1<T>::S1;
  };

int main() {
  S2<int> s; // { dg-error "deleted function" }
}
