// Build don't link:

struct A { // ERROR - inaccessible
  static int i;
};

struct B : private A { };

struct C : public B {
  int f () { return A::i; } // ERROR - context
};
