// { dg-do assemble  }
// { dg-options "-O2 -Winline" }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
inline void f ()
{
  
}

void g ()
{
  f<int> ();
}
