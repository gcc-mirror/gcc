// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  typedef typename T::Y<T>::Z X; // { dg-error "non-template" "non-template" } No Y in A
// { dg-message "note" "note" { target *-*-* } .-1 }
// { dg-error "does not declare" "not declare" { target *-*-* } .-2 }
  X x; // { dg-error "does not name a type" } No Y in A
};

struct A {
  struct Y {
    typedef A Z;
  };
};

template struct S<A>;
