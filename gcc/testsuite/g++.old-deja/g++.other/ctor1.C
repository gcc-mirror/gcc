// { dg-do run  }
// { dg-additional-sources " ctor1-aux.cc" }

// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  template <class U>
  S (U);
};

template <class T>
template <class U>
S<T>::S (U) {}

template S<int>::S (double);
