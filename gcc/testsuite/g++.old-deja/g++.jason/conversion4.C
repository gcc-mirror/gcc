// Build don't link:
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
  f (A (b)); // gets bogus error - functional cast treated differently from C style
}
