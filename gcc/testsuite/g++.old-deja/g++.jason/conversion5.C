// { dg-do assemble  }
// { dg-options "-Wconversion" }
struct A { };
struct B: public A {
  A a;
  operator A () { return a; }	// { dg-warning "" } never used implicitly
};
void f (const A&);
void g()
{
  B b;
  (A) b; // { dg-bogus "" } trying both constructor and type conversion operator
}
