// { dg-do run  }
struct A {
  virtual A* f () { return this; }
};

struct B: public A {
  virtual B* f () { return 0; }
};

int main ()
{
  A* ap = new B;
  return (ap->f () != 0);
}
