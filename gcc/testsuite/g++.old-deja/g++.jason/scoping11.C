// Build don't link:

void f ();
void g ()
{
  int f;
  {
    void f ();
    f ();			// gets bogus error - trying to call integer
  }
}
