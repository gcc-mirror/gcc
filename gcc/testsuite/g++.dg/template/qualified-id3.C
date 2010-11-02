// PR c++/44587

template <const int N> struct A { };
template <class T> struct B {
  static const int c; // { dg-message "not initialized with a constant expression" }
  typedef A<B<T>::c> C;		// { dg-error "constant expression" }
};
template <class T> const int B<T>::c = sizeof (T);

template <const int N> struct D { };
template <class T> struct E {
  static const int c = sizeof (T);
  typedef D<E<T>::c> F;		// OK
};
