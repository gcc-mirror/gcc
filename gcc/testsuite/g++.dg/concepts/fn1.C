// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
  concept bool C() { return __is_class(T); }

struct S { } s;

template<typename T>
  requires C<T>()
    void f(T x) { }

// Calls are valid when arguments are dependent,
template<typename T>
  void g(T x) { f(x); }

// Calls are checked when arguments are non-dependent.
template<typename T>
  void h(T x) {
    f(s);
  }

int main() {
  f(s);
  g(s);
}
