// { dg-do assemble  }
// PRMS id: 4653
// Bug: g++ tries to resolve declarator/expression ambiguities too soon.

template<class T> struct A { };

void f () {
  void (A<int>::*pmf) ();	// { dg-bogus "" } late binding
}
