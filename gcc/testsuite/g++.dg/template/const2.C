// PR c++/39608
// We were improperly considering dependent members of the current
// instantiation to be non-constant (and therefore invalid template
// non-type arguments).

template <int I>
struct C {};

template <class T>
struct A
{
  static const T x = 1;
  C<A<T>::x> c;			// { dg-bogus "invalid" }
};

A<int> a;
