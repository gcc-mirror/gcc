// { dg-do assemble  }
// From: quipu@ulrik.uio.no
// Subject: extern "C" nested class
// Date: Fri, 13 Aug 1993 15:33:53 +0200

extern "C" {
  struct A {
    struct B { int j; } *x;
  };
}

void
foo () {
  A a;
  struct A::B *b;
  b = a.x;	// { dg-bogus "" } type `B' is not a base type for type `B'
}
