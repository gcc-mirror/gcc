// { dg-do compile }
// { dg-options "-Wabi -fabi-version=1" }

struct A {
  virtual void a ();
};

struct B {
  virtual ~B ();
};

struct C : public A, public B { // { dg-warning "virtual" }
  virtual void c ();
};
