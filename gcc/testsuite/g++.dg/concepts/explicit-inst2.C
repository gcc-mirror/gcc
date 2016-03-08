// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() && __is_empty(T); }

struct X { };
struct Y { int n; };

template<typename T> struct S  { void f1() { } };  // #1
template<C T> struct S<T> { void f2() { } };      // #2
template<D T> struct S<T> { void f3() { } };      // #3

template struct S<int>; // Instantiate #1
template struct S<X>; // Instantiate #2
template struct S<Y>; // Instantiate #2

int main() {
  S<int> i; i.f1();
  S<X> x; x.f3();
  S<Y> y; y.f2();
}
