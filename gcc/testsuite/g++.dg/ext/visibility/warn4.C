// Error if we try to give an instantiation visibility after it's already
// been instantiated.

// { dg-require-visibility "" }

template <class T> struct A { void f (T); };
template <class T> void A<T>::f (T) { }

A<double> ad;
template struct __attribute ((visibility ("hidden"))) A<double>; // { dg-error "already defined" }
