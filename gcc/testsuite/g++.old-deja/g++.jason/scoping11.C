// { dg-do assemble  }

void f ();
void g ()
{
  int f;
  {
    void f ();
    f ();			// { dg-bogus "" } trying to call integer
  }
}
