// { dg-do assemble  }

template <class T> struct A {};
template <class T> struct A<T>;    // { dg-error "" } does not specialize args
template <class T> const struct A; // { dg-error "" } parse error
template <class T> template A<int>; // { dg-error "" } .*
