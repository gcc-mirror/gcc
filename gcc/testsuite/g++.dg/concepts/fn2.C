// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  requires C<T>
    void f(T x) { }

// Non-dependent args are checked even in dependent scope.
template<typename T>
  void h(T x) {
    f(0); // { dg-error "" }
  }

int main() {
  f(0); // { dg-error "" }
}
