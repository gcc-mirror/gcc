// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  S() {}

  static S s;
};

template <class T>
S<T> S<T>::s;

S<int> si;
