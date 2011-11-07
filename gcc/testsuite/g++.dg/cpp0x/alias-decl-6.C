// { dg-options "-std=c++0x" }

// Alias template of non-class types.

template <class T, class U> struct same;
template <class T> struct same<T,T> {};

template <class T> using Ptr = T*;
template <template <class> class T> struct A {
  template <class U> using X = T<U>;
};
same<A<Ptr>::X<int>,int*> s;
