// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  typedef typename T::Y<T>::Z X; // { dg-error "not a class template" } No Y in A
  X x;
};

struct A {
  struct Y {
    typedef A Z;
  };
};

template struct S<A>;
