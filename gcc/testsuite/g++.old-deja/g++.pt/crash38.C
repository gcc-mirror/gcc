// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  typedef typename T::Y<T>::Z X; // { dg-error "" } No Y in A
  X x; // { dg-error "" } No Y in A
};

struct A {
  struct Y {
    typedef A Z;
  };
};

template struct S<A>;
