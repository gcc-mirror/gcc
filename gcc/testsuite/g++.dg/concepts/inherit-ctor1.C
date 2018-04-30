// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  struct S1 { S1(double) requires C<T>() { } };

struct S2 : S1<int> {
  using S1<int>::S1;
};

template<typename T>
  struct S3 : S1<T> {
    using S1<T>::S1;
  };

struct X { };

int main() {
  S3<X> s(0.0);
}
