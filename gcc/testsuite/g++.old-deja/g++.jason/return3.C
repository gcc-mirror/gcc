// { dg-do run  }
// PRMS id: 10912

struct A {
  A() { i=10; }
  int i;
};
struct B : public A {};
B b;

A f()
{
  return b;
}

int main ()
{
  A a = f ();
  return a.i != 10;
}
