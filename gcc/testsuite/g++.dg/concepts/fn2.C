// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  requires C<T>()
    void f(T x) { }

// Non-dependent args are checked even in dependent scope.
template<typename T>
  void h(T x) {
    f(0); // { dg-error "" }
  }

int main() {
  f(0); // { dg-error "" }
}
