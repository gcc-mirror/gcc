template <class T0>
class A {
public:
  class B;
};

template <class T0>
class A<T0>::B {
public:
  class C;
};

template <class T0>
class A<T0>::B::C {
public:
  A<T0> &a;
};
