// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

extern "C" void f ();

namespace N {
extern "C" void f ();
}

using N::f;

void g ()
{
  f ();
}
