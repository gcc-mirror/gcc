// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() && __is_empty(T); }

struct X { };
struct Y { int n; };

template<typename T> void g(T) { } // #1
template<C T> void g(T) { } // #2
template<D T> void g(T) { } // #3

// FIXME: How do I test that these generate the right symbols?
template void g(int); // Instantiate #1
template void g(X); // Instantitae #3
template void g(Y); // Instantiate #2

int main() { }
