// { dg-do assemble  }
void f ()
{
  struct A {
    friend void g ();
  };
}
void h () {
  g ();				// { dg-error "" } no g in scope
}
