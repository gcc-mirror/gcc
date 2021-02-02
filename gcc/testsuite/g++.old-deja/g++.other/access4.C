// { dg-do assemble  }

struct A { 
  static int i;
};

struct B : private A { }; // { dg-message "" } inaccessible

struct C : public B {
  int f () { return A::i; } // { dg-error "" } context
};
