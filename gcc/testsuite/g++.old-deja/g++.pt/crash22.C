// { dg-do assemble  }

template <class T>
struct S1 {};

template <class T, class U = S1<T> > 
struct S2 {};

template struct S2<100>; // { dg-error "" } type/value mismatch
