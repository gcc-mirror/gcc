// Bug: member initializers are allowed where they shouldn't be.
// Build don't link:

template <class T>
struct A {
  int i;
  Blarg () : i(0) { }		// ERROR - 
};
