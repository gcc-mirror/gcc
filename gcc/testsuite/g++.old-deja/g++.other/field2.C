// { dg-do run  }
// Test for proper handling of field calls.
// Contributed by Jason Merrill <jason@cygnus.com>

struct A {
  inline A* operator()() { return this; }
};

struct B {
  int i;
  union { A a; };
};

int
main ()
{
  B b;
  A* ap = &b.a;
  A* ap2 = b.a();
  return (ap != ap2);
}
