// Check that we allow an exception specification on a reference-to-function.
// { dg-do compile }

void f () throw ();
void (&fp)() throw () = f;
