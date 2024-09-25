// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Check equivalence of short- and longhand declarations.
// NB: they are not equivalent in C++20.

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = __is_empty(T);

struct X { } x;

void f1(C auto x);
template<C T> void f2(T x);
void f3(C auto x);
template<C T> void f4(T x) requires D<T>;
template<C T> void f5(T x) requires D<T>;
template<C T> void f6(T x) requires D<T>;

template<typename T> requires C<T> void f1(T x) { }
template<typename T> requires C<T> void f2(T x) { }
template<C T> void f3(T x) { }
template<typename T> requires C<T> void f4(T x) requires D<T> { }
template<typename T> requires C<T> and D<T> void f5(T x) { }
template<typename T> void f6(T x) requires C<T> and D<T> { }

int main() {
  f1(x);    // { dg-error "ambiguous" }
  f2(x);    // { dg-error "ambiguous" }
  f3(x);    // { dg-error "ambiguous" }
  f4(x);    // { dg-error "ambiguous" }
  f5(x);    // { dg-error "ambiguous" }
  f6(x);    // { dg-error "ambiguous" }
}
