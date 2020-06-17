// { dg-do compile { target c++20 } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> && __is_empty(T);

template<typename T>
  struct S {
    void g() requires C<T> { } // #1
    void g() requires D<T> { } // #2
  };

template void S<int>::g(); // { dg-error "match" }

int main() { }
