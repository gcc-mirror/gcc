// { dg-do assemble  }
void f ()
{
  struct A {
    friend void g ();		// { dg-error "without prior declaration" }
  };
}
void h () {
  g ();				// { dg-error "" } no g in scope
}
