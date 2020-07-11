// { dg-do compile { target c++20 } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> && __is_empty(T);

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
