// { dg-do assemble  }
// PRMS Id: 4955

struct A {
 protected:
  int i;
  void f ();
};

struct B: public A {
  void g () {
    this->A::i = 1;		// { dg-bogus "" } access control failure
    this->A::f();		// { dg-bogus "" } access control failure
  }
};
