// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -O2

inline void f ()
{
  return;
}

inline void g ();

void (*gp)() = &g;

inline void g ()
{
  f ();
}

extern int array_size;

void h ()
{
  int lookup_array[array_size];
  g ();
}
