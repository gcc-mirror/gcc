// { dg-do assemble  }
void f ()
{
  struct A {
    friend void g ();		// { dg-error "without prior declaration" }
  };
}
void h () {
  g ();				// { dg-error "3:'g' was not declared" } no g in scope
}
