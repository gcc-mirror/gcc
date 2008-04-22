// PR c++/35747
// { dg-do compile }
// { dg-options "" }

void
foo ()
{
  ({ i; ({ i; }); 0; });	// { dg-error "was not declared" }
}
