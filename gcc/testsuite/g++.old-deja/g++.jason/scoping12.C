// Build don't link:
void f ()
{
  struct A {
    friend void g ();
  };
}
void h () {
  g ();				// ERROR - no g in scope
}
