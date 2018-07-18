// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    struct S { };

struct X { };

S<int> sx; // { dg-error "constraint|invalid" }

int main() { }
