// Test for scope-based hiding of functions.

void f (char *);
struct A {
  void f ();			// ERROR - referred to 
};
struct B : public A {
  void g (char *);
  void h () {
    extern void g ();		// ERROR - 
    f("foo");			// ERROR - hidden
    g("foo");			// ERROR - hidden
  }
};
