// PRMS id: 4653
// Bug: g++ tries to resolve declarator/expression ambiguities too soon.
// Build don't link:

template<class T> struct A { };

void f () {
  void (A<int>::*pmf) ();	// gets bogus error - late binding
}
