// { dg-do link { target c++17_only } }
// { dg-options "-fconcepts" }

// Check equivalence of short- and longhand declarations.

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return __is_empty(T); }

struct X { } x;

void f1(C x);
template<C T> void f2(T x);
void f3(C x);
template<C T> void f4(T x) requires D<T>();
template<C T> void f5(T x) requires D<T>();
template<C T> void f6(T x) requires D<T>();

int main() {
  f1(x);
  f2(x);
  f3(x);
  f4(x);
  f5(x);
  f6(x);
}

template<typename T> requires C<T>() void f1(T x) { }
template<typename T> requires C<T>() void f2(T x) { }
template<C T> void f3(T x) { }
template<typename T> requires C<T>() void f4(T x) requires D<T>() { }
template<typename T> requires C<T>() and D<T>() void f5(T x) { }
template<typename T> void f6(T x) requires C<T>() and D<T>() { }
