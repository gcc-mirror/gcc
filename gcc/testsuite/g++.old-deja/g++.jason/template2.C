// { dg-do assemble  }
// Bug: instantiation of member templates breaks.

template <class T> struct A {
  static void f ();
  void g ();
};

template <class T> void A<T>::f () { }
template <class T> void A<T>::g () { }

A<int> a;
