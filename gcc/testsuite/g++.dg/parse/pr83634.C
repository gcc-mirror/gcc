// PR c++/83634
// { dg-do compile }

void
foo ()
{
  const int x = fn ();		// { dg-error "was not declared in this scope" }
  short n;
  for (n = x; n < 100; ++n)
    ;
}
