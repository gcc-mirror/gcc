// PR c++/55058

template <typename T>
struct A { };

template <typename T>
struct B {
  B(const A<T> T::* p);
  typedef A<T> D;
};

template <typename T>
B<T>::B(const D T::* p) { }

struct C {
  C() : e() {};

  const A<C> e;
};

B<C> g(&C::e);
