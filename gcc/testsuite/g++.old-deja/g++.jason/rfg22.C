// { dg-do assemble  }
static void f ();		// { dg-error "" } used but not defined

void g ()
{
  f ();
}
