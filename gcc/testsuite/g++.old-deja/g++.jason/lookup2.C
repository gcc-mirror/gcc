// Build don't link:

struct A {
  struct B {
    B ();
  };
};
void f (A::B);
void g ()
{
  A::B b;
  f (b);	  // gets bogus error - can't find nested constructor
}
