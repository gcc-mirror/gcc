// { dg-do compile }
// { dg-options "-Wabi" }

struct A {
  virtual void a ();
};

struct B {
  virtual ~B ();
};

struct C : public A, public B { // { dg-warning "virtual" }
  virtual void c ();
};
