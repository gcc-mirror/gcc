// PRMS Id: 6604
// Old bug: Scoped constructor call is not properly recognized as a functional cast
// But after DR 147 A::A() is a constructor call, not a functional cast.

int c;

struct A {
  A() { ++c; }
  ~A() { --c; }
  operator int () { return 1; }
};

int main ()
{
  A a;
  a.A::A();			// { dg-error "" }
  A::A();			// { dg-message "" }
  return c;
}
