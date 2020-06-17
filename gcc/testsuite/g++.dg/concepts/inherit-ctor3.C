// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  struct S1 {
    template<C U>
      S1(U x) { }
  };

template<typename T>
  struct S2 : S1<T> { // { dg-error "no matching function" }
    using S1<T>::S1;
  };

struct X { } x;

int main() {
  S2<X> s1(0); // { dg-error "no matching function" }
  S2<X> s2; // { dg-error "use of deleted function" }
}
