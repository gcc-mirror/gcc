// { dg-do assemble  }
struct A {
  A (int);
};
struct B {
  operator int () { return 1; }
};
void f (const A&);
void g()
{
  B b;
  f ((A) b);
  f (A (b)); // { dg-bogus "" } functional cast treated differently from C style
}
