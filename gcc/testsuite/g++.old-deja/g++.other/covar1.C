// { dg-do assemble  }
// Origin: Jason Merrill <jason@cygnus.com>

struct A {
  virtual A& f();
};

struct B: public A {
  B& f();
};
