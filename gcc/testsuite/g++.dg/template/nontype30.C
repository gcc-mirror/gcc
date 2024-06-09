// PR c++/113644

template<int> struct A { };

template<class T> void f(A<42>);
template<class T> void f(A<T::value>);

struct B { static const int value = 42; };

int main() {
  A<42> a;
  f<B>(a); // { dg-error "ambiguous" }
}
