// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C = __is_class(T);

struct X { };

template<typename T>
  struct Base {
    Base(double) requires C<T> { } 
  };

struct Ok1 : Base<X> {
  using Base<X>::Base;
};

struct Err1 : Base<int> {
  using Base<int>::Base;
};

template<typename T>
  struct Generic : Base<T> {
    using Base<T>::Base;
  };


int main() {
  Ok1 x1(0.0);
  Err1 x2(0.0); // { dg-error "no matching" }
  Generic<X> x3(0.0);
  Generic<int> x4(0.0); // { dg-error "no matching" }
}
