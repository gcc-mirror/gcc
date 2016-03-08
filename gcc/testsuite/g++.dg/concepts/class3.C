// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

// Check class redeclaration with alternative spellings.
template<typename T> requires C<T>() struct S;
template<C T> struct S { };

struct X { };

// S<X> sx;

int main() { }
