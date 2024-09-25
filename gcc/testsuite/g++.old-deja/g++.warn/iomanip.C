// { dg-do assemble  }
// { dg-options "-O -Winline" }
// { dg-skip-if "requires hosted libstdc++ for iomanip" { ! hostedlib } }
// Bug: g++ wouldn't inline op<< because it was an explicit instantiation.
// Origin: Jason Merrill <jason@cygnus.com>

#include <iomanip>
#include <iostream>

void
f()
{
  std::cout << std::setw(3);
}
