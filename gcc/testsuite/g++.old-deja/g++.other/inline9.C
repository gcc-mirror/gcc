// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Mark Mitchell <mitchell@codesourcery.com>

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

