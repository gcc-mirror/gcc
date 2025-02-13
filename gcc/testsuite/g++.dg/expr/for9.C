// PR c++/118822
// { dg-do compile }

struct A { A (); ~A (); };
bool baz ();

void
foo ()
{
  while (bool x = baz ())
    {
    lab:;
      A a;
    }
}

void
bar ()
{
  for (bool y = baz (); bool x = baz (); y |= x)
    {
    lab:;
      A a;
    }
}
