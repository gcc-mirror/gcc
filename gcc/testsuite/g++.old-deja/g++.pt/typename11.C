// Build don't link:
// Special g++ Options:
// excess errors test - XFAIL *-*-*

template <class T, int I>
struct S {
  struct X {};
};

template <class T, class U, int I>
S<T,I>::X f(T, U)
{
  S<T, I>::X();
}

template S<int, double>::X f<int, double, 3>(int, double);
