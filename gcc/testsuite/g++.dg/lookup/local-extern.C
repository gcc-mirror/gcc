int foo ()
{
  extern int baz (int i = 5);
  return baz ();
}

int baz (int i = 0);

int bar ()
{
  extern int baz (int i = 6);
  return baz ();
}
