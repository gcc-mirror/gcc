// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

template <class T> struct A { // { dg-error "" } shadowed parameter
  struct B {
    void T(); // { dg-error "" } shadows template parameter
  };
};
A<int> a;

