// { dg-do assemble  }

template <class X> struct A {  };
template <> struct A<int> { int foo (); };
int A<int>::foo () { return 37; }
