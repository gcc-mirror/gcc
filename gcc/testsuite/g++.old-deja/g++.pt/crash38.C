// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  typedef typename T::Y<T>::Z X; // ERROR - No Y in A
  X x; // ERROR - No Y in A
};

struct A {
  struct Y {
    typedef A Z;
  };
};

template struct S<A>;
