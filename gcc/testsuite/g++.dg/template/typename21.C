// PR c++/70067
// { dg-do compile }

template <class> struct A;
template <class T> struct B { struct N { }; };
template <class T> struct D: B<T> {
  typedef typename D::N N;
  A<N> *a;
};

D<int> d;
