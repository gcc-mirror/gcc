// PRMS Id: 4484 (bug 3)
// Bug: g++ does implicitly take the address of methods passed to fns.
// Build don't link:

struct A {
  void f ();
};

void g (void (A::*)());

void h () {
  g (A::f);			// ERROR - failed conversion to method pointer
}
