// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool C() { return __is_class(T); }

struct X { };

template<C T> struct S;
template<> struct S<X> { void f() { } };

int main() {
  S<X> x; x.f();
}
