// { dg-do assemble  }

struct A {
  struct B {
    B ();
  };
};
void f (A::B);
void g ()
{
  A::B b;
  f (b);	  // { dg-bogus "" } can't find nested constructor
}
