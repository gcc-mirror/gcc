// { dg-do compile { target c++2a } }

template<typename T>
  concept C = __is_class(T);

struct X { };

template<C T> struct S;
template<> struct S<X> { void f() { } };

int main() {
  S<X> x; x.f();
}
