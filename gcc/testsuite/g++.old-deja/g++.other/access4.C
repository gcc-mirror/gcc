// Build don't link:

struct A {
  static int i;
};

struct B : private A { };

struct C : public B {
  int f () { return A::i; }
};
