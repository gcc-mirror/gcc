// { dg-do assemble  }
// { dg-options "-Woverloaded-virtual" }

struct A {
  virtual void f(); // { dg-warning "" } hidden 
};

struct B: public A {
  void f(int); // { dg-warning "" } by this
};
