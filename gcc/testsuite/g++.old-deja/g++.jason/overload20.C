// Bug: this code causes an internal compiler error 4.

void f (char *);
void f (int);
struct A {
  void f ();			// ERROR - candidate
  void f (int);			// ERROR - candidate
  void g () {
    void (*p)(char *) = f;	// ERROR - no matching function in scope
  }
};
