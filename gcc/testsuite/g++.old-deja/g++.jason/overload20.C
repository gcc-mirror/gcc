// Bug: this code causes an internal compiler error 4.

void f (char *);
void f (int);
struct A {
  void f ();
  void f (int);
  void g () {
    void (*p)(char *) = f;	// ERROR - no matching function in scope
  }
};
