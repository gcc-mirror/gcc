// Build don't link:
// Special g++ Options: -O2
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S 
{
};

void f (S s = S ());

inline void g ()
{
  f ();
}

void h ()
{
  g ();
}
