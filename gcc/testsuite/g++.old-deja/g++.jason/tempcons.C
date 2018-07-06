// { dg-do assemble  }
// Bug: member initializers are allowed where they shouldn't be.

template <class T>
struct A {
  int i;
  Blarg () : i(0) { return 0; }		// { dg-error "" } 
};
