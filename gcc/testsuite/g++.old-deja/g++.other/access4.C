// { dg-do assemble  }

struct A { // { dg-error "" } inaccessible
  static int i;
};

struct B : private A { };

struct C : public B {
  int f () { return A::i; } // { dg-error "" } context
};
