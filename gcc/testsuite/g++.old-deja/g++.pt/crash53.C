// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S 
{
};

S g ();

template <class T>
void f ()
{
  const S& s = g ();
}

template void f<int>();
