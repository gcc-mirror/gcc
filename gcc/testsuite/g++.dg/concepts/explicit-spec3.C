// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<C T> struct S;

struct X { };

// Not a valid explicit specialization, int does not satisfy C.
template<> struct S<int> { }; // { dg-error "constraint" }

int main() { }
