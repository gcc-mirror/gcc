// Build don't link:
// Special g++ Options: -O2 -Winline
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
inline void f ()
{
  
}

void g ()
{
  f<int> ();
}
