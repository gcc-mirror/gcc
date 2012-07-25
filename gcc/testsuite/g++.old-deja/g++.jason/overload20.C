// { dg-do assemble  }
// Bug: this code causes an internal compiler error 4.

void f (char *);
void f (int);
struct A {
  void f ();			// { dg-message "" } candidate
  void f (int);			// { dg-message "" } candidate
  void g () {
    void (*p)(char *) = f;	// { dg-error "" } no matching function in scope
  }
};
