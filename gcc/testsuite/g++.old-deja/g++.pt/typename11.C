// Build don't link:
// Special g++ Options: -Wno-deprecated

template <class T, int I>
struct S {
  struct X {};
};

template <class T, class U, int I>
S<T,I>::X f(T, U) {   // WARNING - implicit typename
  S<T, I>::X();
  return S<T, I>::X();
}

template S<int, 3>::X f<int, double, 3>(int, double);
