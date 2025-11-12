// { dg-additional-options "-fmodules" }

export module M;

export {
template <class T>
int fn() {
  return T::mem;
}

template <class T>
class A {
  inline static int mem = 42;
  friend int fn<A>();
};

template <class T>
class B {
  inline static int mem = 24;
  friend int fn<B>();
};
}
