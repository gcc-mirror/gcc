// Red Hat bugzilla 64535
// Bug: We are allocationg stuff into the tail padding of POD class "A".
// { dg-do run }

struct A
{
  int x;
  char y;
};

struct B : public A {
  virtual void f () {}
  char z;
};

A a = { 21, 42 };
B b;

int
main (void)
{
  b.x = 12;
  b.y = 24;
  b.z = 36;

  A *ap = &b;

  *ap = a;

  return (b.z != 36);
}
