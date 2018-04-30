// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() and __is_empty(T); }

struct S1 { } s1;
struct S2 { int n; } s2;

template<C T> void f1(T x) { }
template<D T> void f1(T x) { }

int main() {
  f1(0); // { dg-error "matching" }
}
