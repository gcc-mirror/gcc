// Bug: instantiation of member templates breaks.
// Build don't link:

template <class T> struct A {
  static void f ();
  void g ();
};

template <class T> void A<T>::f () { }
template <class T> void A<T>::g () { }

A<int> a;
