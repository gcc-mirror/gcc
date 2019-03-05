// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    struct S { };

struct X { };

S<int> sx; // { dg-error "constraint|invalid" }

int main() { }
