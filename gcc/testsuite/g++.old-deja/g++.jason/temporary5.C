// { dg-do run  }
// PRMS Id: 6604
// Bug: Scoped constructor call is not properly recognized as a functional cast

int c;

struct A {
  A() { ++c; }
  ~A() { --c; }
  operator int () { return 1; }
};

int main ()
{
  A::A();
  return c;
}
