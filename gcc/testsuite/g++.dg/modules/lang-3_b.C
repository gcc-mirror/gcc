// { dg-additional-options -fmodules-ts }
import bob;

void Foo () 
{
  Bar ();
  Baz (); // { dg-error "was not declared" }
  Quux ();
}

void Bar ();
void Baz ();

void Quux ()
{
  Bar ();
  Baz ();
}
