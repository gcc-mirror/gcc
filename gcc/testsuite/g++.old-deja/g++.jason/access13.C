// PRMS Id: 4955
// Build don't link:

struct A {
 protected:
  int i;
  void f ();
};

struct B: public A {
  void g () {
    this->A::i = 1;		// gets bogus error - access control failure
    this->A::f();		// gets bogus error - access control failure
  }
};
