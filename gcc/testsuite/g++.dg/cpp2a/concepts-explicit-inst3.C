// { dg-do compile { target c++20 } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> && __is_empty(T);

struct X { };
struct Y { int n; };

template<typename T>
  struct S {
    void f() { }                 // #1
    void f() requires C<T> { } // #2

    void g() requires C<T> { } // #1
    void g() requires D<T> { } // #2
  };

template void S<int>::f(); // #1
template void S<X>::f(); // #2

template void S<X>::g(); // #2
template void S<Y>::g(); // #1

int main() { }
