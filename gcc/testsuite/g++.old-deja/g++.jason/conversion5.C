// Build don't link:
// Special Options: -Wconversion
struct A { };
struct B: public A {
  A a;
  operator A () { return a; }	// WARNING - never used implicitly
};
void f (const A&);
void g()
{
  B b;
  (A) b; // gets bogus error - trying both constructor and type conversion operator
}
