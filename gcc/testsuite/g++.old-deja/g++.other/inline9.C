// Build don't link:
// Origin: Mark Mitchell <mitchell@codesourcery.com>
// Special g++ Options: -O2

inline void f ()
{
  int n;
  int i[n];
}

void g ()
{
  f ();
}

void h ()
{
  f ();
}

