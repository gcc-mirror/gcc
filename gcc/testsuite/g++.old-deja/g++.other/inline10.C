// Build don't link:
// Special g++ Options: -O2
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
