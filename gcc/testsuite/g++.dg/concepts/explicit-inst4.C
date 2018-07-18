// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() && __is_empty(T); }

template<typename T>
  struct S {
    void g() requires C<T>() { } // #1
    void g() requires D<T>() { } // #2
  };

template void S<int>::g(); // { dg-error "match" }

int main() { }
