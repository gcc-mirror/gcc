// { dg-do compile { target c++20 } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  struct S1 {
    S1(double) requires C<T> { }
  };

template<typename T>
  struct S2 : S1<T> { // { dg-error "matching" }
    using S1<T>::S1;
  };

int main() {
  S2<int> s; // { dg-error "deleted function" }
}
