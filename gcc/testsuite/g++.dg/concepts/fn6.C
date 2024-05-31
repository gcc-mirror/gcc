// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Redefinition errors.

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> and __is_empty(T);

template<C T> void f(T x) { }
template<typename T>
  requires C<T>
    void f(T x) { }

int main() { }
