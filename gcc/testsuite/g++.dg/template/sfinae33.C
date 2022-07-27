// PR c++/105351

template<int> struct A { };

template<class T> A<T::value> f();
template<class T> void f();

struct B { int value; };

int main() {
  f<B>();
}
