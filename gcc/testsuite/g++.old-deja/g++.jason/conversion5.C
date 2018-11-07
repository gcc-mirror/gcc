// { dg-do assemble  }
// { dg-options "-Wconversion" }
struct A { };
struct B: public A {
  A a;
  operator A () { return a; }  // { dg-warning "3:converting .B. to a base class .A. will never use a type conversion operator" }
};
void f (const A&);
void g()
{
  B b;
  (A) b; // { dg-bogus "" } trying both constructor and type conversion operator
}
