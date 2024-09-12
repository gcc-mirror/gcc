// { dg-do run  }
// { dg-options "-O2" }
// { dg-skip-if "requires hosted libstdc++ for list" { ! hostedlib } }
// Origin: Mark Mitchell <mark@codesourcery.com>

#include <list>

std::list<int*> li;

void f ()
{
  (void) li.size ();
}

int main ()
{
  li.push_back (0);
  f ();
}
