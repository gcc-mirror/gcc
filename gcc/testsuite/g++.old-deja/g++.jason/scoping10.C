// { dg-do assemble  }
// Test for scope-based hiding of functions.

void f (char *);
struct A {
  void f ();			// { dg-message "A::f|candidate expects" } referred to 
};
struct B : public A {
  void g (char *);
  void h () {
    extern void g ();		// { dg-message "" } 
    f("foo");			// { dg-error "" } hidden
    g("foo");			// { dg-error "" } hidden
  }
};
