// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S1 {
  typedef T X;
};

template <class T>
struct B {
  typedef T I;
};

template <class T>
struct S2 : public B<T> {
  struct I {};

  typedef typename S1<I>::X IX;

  void f(IX);
};

template <class T>
void S2<T>::f(IX) {}
