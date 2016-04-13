// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    struct S { };

struct X { };

S<X> sx;

int main() { }
