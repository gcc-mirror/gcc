// { dg-do assemble  }
// PRMS Id: 5070 (bug 2)

struct A {
  void f ();
};

struct Ptr {
  A* operator->();
};

struct B {
  Ptr p;
};

struct C: public B {
  void g ();
};

void C::g() {
  B::p->f();			// { dg-bogus "" } 
}
