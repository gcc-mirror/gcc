// { dg-do assemble  }
// { dg-options "-Wconversion" }
struct A { };
struct B: public A {
  A a;
  operator A () { return a; }  // { dg-warning "3:conversion to a base class will never use a type conversion operator" }
};
void f (const A&);
void g()
{
  B b;
  (A) b; // { dg-bogus "" } trying both constructor and type conversion operator
}
