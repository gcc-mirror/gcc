template <typename T> struct A {};

struct B {
  operator A<B>();
};

template <typename T>
void f() { B::operator A<T>; }
