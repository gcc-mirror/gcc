// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
};

inline const S f () 
{
  return S ();
}

void g ()
{
  S s;
  f ();
}
