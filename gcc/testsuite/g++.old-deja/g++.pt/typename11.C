// { dg-do assemble  }

template <class T, int I>
struct S {
  struct X {};
};

template <class T, class U, int I>
typename S<T,I>::X f(T, U) {
  typename S<T, I>::X();
  return typename S<T, I>::X();
}

template S<int, 3>::X f<int, double, 3>(int, double);
